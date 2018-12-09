package ru.bmstu.bioinformatics.database.converted

import java.nio.ByteBuffer

import boopickle.Default._
import ru.bmstu.bioinformatics.Utils._
import ru.bmstu.bioinformatics.algo.util.DotPlot.SubstringMap
import slick.basic.DatabasePublisher
import slick.jdbc.SQLiteProfile.api._

object DatabaseOperator {

  private lazy val db = Database.forConfig("dnadb")
  private lazy val table = TableQuery[DnaTable]
  private val dnaTableName = "converted"

  def init(): Unit = {
    db.run(DBIO.seq(table.schema.create)).await()
  }

  def count(): Int = {
    db.run(table.length.result).await()
  }

  //indices are assumed to start from zero
  def read(from: Long, to: Long): DatabasePublisher[DbEntry] = {
    db.stream(
      table.sortBy(_.id)
        .map(t => (t.name, t.sequence, t.bob))
        .drop(from)
        .take(to - from - 1).result
    )
      .mapResult { tup => DbEntry((tup._1, tup._2, Unpickle.apply[SubstringMap].fromBytes(ByteBuffer.wrap(tup._3)))) }
  }

  def write(insert: Iterator[DbEntry]): Unit = {
    var size = 0
    for {
      entries <- insert.grouped(100)
    } {
      val i = entries.map { case DbEntry((name, seq, substrings)) => (name, seq, Pickle.intoBytes(substrings).array()) }
      val q = DBIO.seq(table.map(t => (t.name, t.sequence, t.bob)) ++= i)
      db.run(q).await()
      size += i.size
      println(s"Loaded: $size")
    }
  }

  def close(): Unit = {
    db.close()
  }

  private class DnaTable(tag: Tag) extends Table[(Long, String, String, Array[Byte])](tag, dnaTableName) {
    def * = (id, name, sequence, bob)

    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

    def name = column[String]("name")

    def sequence = column[String]("sequence")

    def bob = column[Array[Byte]]("bob")
  }
}
