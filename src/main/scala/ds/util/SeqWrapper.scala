package util

import scala.collection.AbstractSeq


/* Implements [[AbstractSeq]] by delegating to sublasses implemntation of [[toSeq]] */
abstract class SeqWrapper[A] extends AbstractSeq[A] {

  def apply(idx:Int) = toSeq.apply(idx)
  def iterator = toSeq.iterator
  def length = toSeq.length
  
}