package util

case class Tree[A](val root:A)(childs:A=>Seq[A]) {

	/** Traverses tree in depth-first search */ 
	def dfs:Seq[A]= Tree.dfs(root,childs)
	def leaves:Seq[A] = dfs.filter(childs(_).isEmpty)
}

object Tree {

	def dfs[A](root:A, childs:A=>Seq[A]):Seq[A]=
		root +: childs(root).flatMap { c:A => dfs(c,childs)}
}	