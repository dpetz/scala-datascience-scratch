package util

import scala.collection.AbstractMap

/** Delegates methods to sublasses ``toMap´´ */
class MapWrapper[K,+V]() extends AbstractMap[K,V] {
  def get(idx:K):Option[V] = toMap.get(idx)
  def iterator = toMap.iterator
  def +[B >: V](kv:(K,B)) = toMap + kv 
  def -(k:K) = toMap - k

}