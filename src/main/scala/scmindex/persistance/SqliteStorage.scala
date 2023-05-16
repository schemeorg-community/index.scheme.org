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

      def save(lst: List[SCMIndexEntry]): IO[List[(Int, SCMIndexEntry)]] = IO.pure(List())

      def get(id: Int): IO[Option[SCMIndexEntry]] = IO.pure(None)

      def saveFiltersets(f: List[Filterset]): IO[Unit] = {
        val insert = "insert into filterset(code, name) values(?, ?)"
        val params = f.map(filterset => (filterset.code, filterset.name))
        val statements = for {
          _ <- Update[(String, String)](insert).updateMany(params)
        } yield ()
        t.transactor.use(xa => {
          statements.transact(xa)
        })
      }

      def getFiltersets(): IO[List[Filterset]] = {
        val statements = sql"select code, name from filterset"
          .query[(String, String)]
          .map({
            case (code, name) => Filterset(code, name, List())
          })
          .to[List]
        t.transactor.use(xa => {
          statements.transact(xa)
        })
      }
    }
  }
}
