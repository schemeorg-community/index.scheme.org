package scmindex.persistance

import javax.sql.DataSource
import scmindex.core.*
import cats.effect.IO
import org.flywaydb.core.Flyway
import doobie.*
import doobie.implicits.*
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import cats.effect.kernel.Resource
import cats.implicits._
import cats.data.OptionT


case class SqliteStorage(datasource: DataSource, transactor: Resource[IO, Transactor[IO]])

object SqliteStorage {

  def create(location: String): IO[SqliteStorage] = {
    for {
      ds <- IO {
        val cfg = new HikariConfig();
        cfg.setJdbcUrl(s"jdbc:sqlite:${location}")
        new HikariDataSource(cfg)
      }
      transactor = for {
        pool <- ExecutionContexts.fixedThreadPool[IO](2)
      } yield Transactor.fromDataSource[IO](ds, pool)
    } yield SqliteStorage(ds, transactor)
  }

  given Storage[SqliteStorage, Int] with {
    extension(t: SqliteStorage) {

      def init() = IO {
          val flyway = Flyway.configure().dataSource(t.datasource).load()
          flyway.migrate()
          ()
      }

      def deleteAll(): IO[Unit] = {
        val statements = for {
          _ <- sql"delete from filterset_library".update.run
          _ <- sql"delete from filterset".update.run
          _ <- sql"delete from index_entry_tag".update.run
          _ <- sql"delete from index_entry_subsignature".update.run
          _ <- sql"delete from index_entry".update.run
        } yield ()
        t.transactor.use(xa => {
          statements.transact(xa)
        })
      }

      def saveSingle(e: SCMIndexEntry): ConnectionIO[Int] = {
        def saveInternalSingle(e: SCMIndexEntrySingle, groupId: Option[Int]): ConnectionIO[Int] = {
          for {
            id <- sql"insert into index_entry(group_id, lib, name, signature, description) values(${groupId}, ${e.lib}, ${e.name}, ${SCMIndexEntry.serializeSignature(e.signature).write()}, ${e.description})"
              .update
              .withUniqueGeneratedKeys[Int]("id")
            _ <- e.tags.map(tag => {
                sql"insert into index_entry_tag(index_entry_id, name) values(${id}, ${tag})".update.run
              }).sequence
            _ <- e.subsignatures.map(subsig => {
                sql"insert into index_entry_subsignature(index_entry_id, name, signature) values(${id}, ${subsig.paramName}, ${SCMIndexEntry.serializeSubSignature(subsig.signature).write()})"
                  .update
                  .run
              }).sequence
          } yield id
        }
        def saveInternalGroup(e: SCMIndexEntryGroup): ConnectionIO[Int] = {
          for {
            groupId <- sql"insert into index_entry(lib, description) values(${e.lib}, ${e.description})".update.withUniqueGeneratedKeys[Int]("id")
            groupItems <- e.entries.map(entry => saveInternalSingle(entry, Some(groupId))).sequence
          } yield groupId
        }
        e match {
          case group: SCMIndexEntryGroup => saveInternalGroup(group)
          case single: SCMIndexEntrySingle => saveInternalSingle(single, None)
        }
      }

      def save(lst: List[SCMIndexEntry]): IO[List[(Int, SCMIndexEntry)]] = {
        val statements = lst.map(entry => saveSingle(entry).map(id => (id, entry))).sequence
        t.transactor.use(xa => {
          statements.transact(xa)
        })
      }

      def glueIndexEntry(entries: List[(Int, Option[Int], String, Option[String], Option[String], Option[String])], tags: List[(Int, String)], subsigs: List[(Int, String, String)]): Either[Exception, Option[SCMIndexEntry]] = {
        val tagsMap = tags.groupMap(e => e._1)(e => e._2)
        val subsigsMap = subsigs.groupMap(e => e._1)(e => for {
            sigSexpr <- SexprParser.read(e._3)
            sig <- SCMIndexEntry.doParseSubsig(e._2, sigSexpr)
        } yield sig).mapValues(lst => lst.sequence)
        def rowToEntrySingle(e: (Int, Option[Int], String, Option[String], Option[String], Option[String])): Either[Exception, SCMIndexEntrySingle] = {
          for {
            sexprString <- e._5.toRight(Exception("Missing signature"))
            name <- e._4.toRight(Exception("Missing name"))
            lib = e._3
            sigSexpr <- SexprParser.read(sexprString)
            sig <- SCMIndexEntry.parseSignature(sigSexpr)
            subsigs <- subsigsMap.get(e._1) match {
              case Some(e) => e
              case None => Right(List())
            }
            tags = tagsMap.get(e._1) match {
              case Some(t) => t
              case None => List()
            }
            description = e._6.getOrElse("")
          } yield SCMIndexEntrySingle(name, lib, sig, subsigs, tags, description)
        }
        val maybeGroupRow = entries.filter(e => e._2.isEmpty)
        val nonGroupRows = entries.filter(e => e._2.isDefined)
        (maybeGroupRow, nonGroupRows) match {
          case (List(), List()) => Right(None)
          case (List(maybeGroupRow), List()) => rowToEntrySingle(maybeGroupRow).map(Some.apply)
          case (List(groupRow), lst) => for {
            internalEntries <- lst.map(rowToEntrySingle).sequence
          } yield Some(SCMIndexEntryGroup(groupRow._3, internalEntries, groupRow._6.getOrElse("")))
          case _ => Left(Exception("Wrong hierarchy"))
        }
      }

      def get(id: Int): IO[Option[SCMIndexEntry]] = {
        val statements = for {
          entries <- sql"select id, group_id, lib, name, signature, description from index_entry where id = $id or group_id = $id"
            .query[(Int, Option[Int], String, Option[String], Option[String], Option[String])]
            .to[List]
          tags <- sql"select index_entry_id, name from index_entry_tag where index_entry_id = $id or index_entry_id in (select id from index_entry where group_id = $id)"
            .query[(Int, String)]
            .to[List]
          subsigs <- sql"select index_entry_id, name, signature from index_entry_subsignature where index_entry_id = $id or index_entry_id in (select id from index_entry where group_id = $id)"
            .query[(Int, String, String)]
            .to[List]
        } yield glueIndexEntry(entries, tags, subsigs)
        t.transactor.use(xa => {
          for {
            result <- statements.transact(xa)
            unwrapped <- IO.fromEither(result)
          } yield unwrapped
        })
      }

      def saveFilterset(f: Filterset): ConnectionIO[Unit] = {
        val code = f.code
        val name = f.name
        for {
          id <- sql"insert into filterset(code, name) values($code, $name)".update.withUniqueGeneratedKeys[Int]("id")
          _ <- Update[(Int, String)]("insert into filterset_library(filterset_id, name) values(?, ?)").updateMany(f.libs.map(l => (id, l)))
        } yield()
      }

      def saveFiltersets(lst: List[Filterset]): IO[Unit] = {
       val statements = lst.map(saveFilterset).sequence_
       t.transactor.use(xa => {
         statements.transact(xa)
       })
      }

      def getFiltersets(): IO[List[Filterset]] = {
        val statements = sql"select f.code, f.name, l.name from filterset f, filterset_library l where f.id = l.filterset_id"
          .query[(String, String, String)]
          .to[List]
        t.transactor.use(xa => {
          statements.transact(xa).map(rows => {
            rows.groupMap(e => (e._1, e._2))(e => e._3).toList.map(e => Filterset(e._1._1, e._1._2, e._2))
          })
        })
      }
    }
  }
}
