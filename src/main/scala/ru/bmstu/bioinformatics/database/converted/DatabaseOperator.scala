package ru.bmstu.bioinformatics.database.converted

import java.io._
import java.nio.ByteBuffer

import boopickle.Default._
import ru.bmstu.bioinformatics.Utils
import slick.basic.DatabasePublisher
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object DatabaseOperator {

  private val dnaTableName = "converted"
  private class DnaTable(tag: Tag) extends Table[(Long, String, Array[Byte])](tag, dnaTableName) {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def bob = column[Array[Byte]]("bob")
    def * = (id, name, bob)
  }

  private lazy val db = Database.forConfig("dnadb")
  private lazy val table = TableQuery[DnaTable]

  def init(): Future[Unit] = {
    db.run(DBIO.seq(table.schema.create))
  }

  def read(): DatabasePublisher[DbEntry] = {
    val q = for(dna <- table) yield (dna.name, dna.bob)
    db.stream(q.result).mapResult {
      case (name, bob) => DbEntry((name, Unpickle.apply[Vector[String]].fromBytes(ByteBuffer.wrap(bob))))
    }
  }

  def write(insert: Iterator[DbEntry]): Unit = {
    for {
      entries <- insert.grouped(10)
    } {
      val i = entries.map { case DbEntry((name, substrings)) => (name, Pickle.intoBytes(substrings).array()) }
      val q = DBIO.seq(table.map(t => (t.name, t.bob)) ++= i)
      Await.result(db.run(q), Duration.Inf)
      println("done")
    }
  }
}
