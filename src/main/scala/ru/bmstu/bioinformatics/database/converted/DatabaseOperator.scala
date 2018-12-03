package ru.bmstu.bioinformatics.database.converted

import java.nio.ByteBuffer

import boopickle.Default._
import ru.bmstu.bioinformatics.Utils._
import slick.basic.DatabasePublisher
import slick.jdbc.SQLiteProfile.api._

object DatabaseOperator {

  private lazy val db = Database.forConfig("dnadb")
  private lazy val table = TableQuery[DnaTable]
  private val dnaTableName = "converted"

  def init(): Unit = {
    db.run(DBIO.seq(table.schema.create)).await()
  }

  def read(): DatabasePublisher[(Long, DbEntry)] = {
    val q = for (dna <- table) yield (dna.id, dna.name, dna.bob)
    db.stream(q.result).mapResult {
      case (id, name, bob) => id -> DbEntry((name, Unpickle.apply[Vector[String]].fromBytes(ByteBuffer.wrap(bob))))
    }
  }

  def write(insert: Iterator[DbEntry]): Unit = {
    var size = 0
    for {
      entries <- insert.grouped(100)
    } {
      val i = entries.map { case DbEntry((name, substrings)) => (name, Pickle.intoBytes(substrings).array()) }
      val q = DBIO.seq(table.map(t => (t.name, t.bob)) ++= i)
      db.run(q).await()
      size += i.size
      println(s"Loaded: $size")
    }
  }

  private class DnaTable(tag: Tag) extends Table[(Long, String, Array[Byte])](tag, dnaTableName) {
    def * = (id, name, bob)

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def name = column[String]("name")

    def bob = column[Array[Byte]]("bob")
  }
}
