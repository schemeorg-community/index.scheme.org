package scmindex.persistance

import scmindex.core.*
import javax.sql.DataSource
import cats.effect.IO
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import scala.util.Using

case class SqliteStorage(source: DataSource)

object SqliteStorage {

  def create(location: String) = {
    val cfg = new HikariConfig();
    cfg.setJdbcUrl(s"jdbc:sqlite:${location}")
    SqliteStorage(new HikariDataSource(cfg))
  }

  given Storage[SqliteStorage, Int] with {
    extension(t: SqliteStorage) {
      def init() = IO {
        Using.Manager { use =>
          val con = use(t.source.getConnection())
          con.setAutoCommit(false)
          val statement = con.createStatement()
          statement.execute(createTablesStatements)
          con.commit()
        }
      }
      def get(id: String): IO[Option[SCMIndexEntry]] = ???
      def save(lst: List[SCMIndexEntry]): IO[Unit] = ???
    }
  }

  val createTablesStatements = """
    create table if not exists index_entry(
      name TEXT,
      desc TEXT
    );
  """

}
