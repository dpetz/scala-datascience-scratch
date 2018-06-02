package math.lina

import scala.collection.AbstractMap

import util.MapWrapper

trait Entries[A] extends Matrix[A] with Map[(Int,Int),A]

/** Wraps matrix as [[Map]] */

object Entries {

  def apply[A](mat:Matrix[A]):Entries[A] = Matrix2Entries(mat)

  def apply[A](map:Map[(Int,Int),A]):Entries[A] = Map2Entries(map)




  case class Map2Entries[A] (toMap:Map[(Int,Int),A]) extends MapWrapper[(Int,Int),A] with Entries[A] {
      override def apply(i:Int,j:Int) = toMap((i,j))
    }  

  case class Matrix2Entries[A](mat:Matrix[A]) extends AbstractMap[(Int,Int),A] with Entries[A] {

    ///// Matrix methods /////

    override def rows    = mat.rows
    override def columns = mat.columns
    override def apply(i:Int,j:Int) = mat(i,j)

    ///// Mandatory Map methods /////

    def get(idx:(Int, Int)):Option[A] = 
      try { Some(mat(idx._1, idx._2)) }
      catch { case e:Exception => None }
    
    def iterator = mat.rows.zipWithIndex.flatMap {
      case (row,i) => row.zipWithIndex.map {
        case (x,j) => ((i,j),x)
      }}.iterator

    /** @throws UnsupportedOperationException as result not rectangular. */
    def +[B >: A](kv:((Int, Int),B)):Map[(Int, Int),B] = throw new UnsupportedOperationException

    /** @throws UnsupportedOperationException as result not rectangular. */
    def -(k:(Int, Int)) = throw new UnsupportedOperationException

    ///// More Map methods /////

    def map[B](f:(((Int,Int),A))=>((Int,Int),B)):Entries[B] = Entries(super.map(f))

  }
}