package math.lina

import io.Json

import scala.collection.{Map,AbstractMap}


/**
  * Subclasses must implement at least [[rows]]
  */
trait Matrix[A] {
   

  /** Returns number of rows followed by number of columns. */
  def apply(i:Int,j:Int):A = rows(i)(j)
  
  def columns:Columns[A] = this match {
    case c:Columns[A] => c
    case _            => Columns(this)
  }
  
  def rows:Rows[A] = this match {
    case r:Rows[A] => r
    case _         => Rows(this)
  }


  def entries:Entries[A] =  this match {
    case e:Entries[A] => e
    case _            => Entries(this)
  }




  override def toString = s"Matrix(${rows.size} rows, ${columns.size} columns)"
}
  
object Matrix {


}
