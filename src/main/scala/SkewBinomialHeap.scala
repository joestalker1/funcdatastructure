package utils

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

class SkewBinomialHeap[+A <% Ordered[A]] private(private[utils] val heap: SkewBinomialHeap.SkewHeap[A]) {

  import SkewBinomialHeap._

  def isEmpty(): Boolean = heap.isEmpty

  def insert[B >: A <% Ordered[B]](x: B): SkewBinomialHeap[B] = new SkewBinomialHeap(insert(x, heap))

  private def insert[B >: A <% Ordered[B]](x: B, skewHeap: SkewHeap[B]): SkewHeap[B] = skewHeap match {
    case t1 :: t2 :: rest if rank(t1) == rank(t2) => skewLink(x, t1, t2) :: rest
    case _ => Node(0, x, Nil, Nil) :: skewHeap
  }

  def findMin(): Option[A] = removeMinTree[A](heap).map(_._1).map(root[A])

  def deleteMin(): Option[SkewBinomialHeap[A]] = {
    val ifNewHeap = removeMinTree(heap).map { case (Node(_, x, xs, ts1), ts2) =>
      insertAll(xs, merge(ts1.reverse, ts2))
    }
    ifNewHeap.map(new SkewBinomialHeap(_))
  }

  private def insertAll[B >: A <% Ordered[B]](elems: List[B], ts2: SkewHeap[B]): SkewHeap[B] = (elems, ts2) match {
    case (x :: xs, h) => insertAll(xs, insert(x, h))
    case (Nil, h) => h
  }


  private def insTree[B >: A <% Ordered[B]](t1: Node[B], h: SkewHeap[B]): SkewHeap[B] = h match {
    case Nil => List(t1)
    case t2 :: ts => if (rank(t1) < rank(t2)) t1 :: t2 :: ts
    else insTree(link(t1, t2), ts)
  }

  private def normalize[B >: A <% Ordered[B]](t: SkewHeap[B]): SkewHeap[B] = t match {
    case t :: ts => insTree(t, ts)
    case Nil => Nil
  }

  private def merge[B >: A <% Ordered[B]](ts1: SkewHeap[B], ts2: SkewHeap[B]): SkewHeap[B] =
    mergeTrees(normalize(ts1), normalize(ts2))

  private def removeMinTree[B >: A <% Ordered[B]](ts: SkewHeap[B]): Option[(Node[B], SkewHeap[B])] = ts match {
    case List(node) => Some((node, Nil))
    case t :: rest => removeMinTree(rest).map { case (t1, ts1) =>
      if (root(t) <= root(t1)) (t, rest) else (t1, t :: ts1)
    }
    case Nil => None
  }

  private def mergeTrees[B >: A <% Ordered[B]](ts1: SkewHeap[B], ts2: SkewHeap[B]): SkewHeap[B] = (ts1, ts2) match {
    case (ts1, Nil) => ts1
    case (Nil, ts2) => ts2
    case (t1 :: tsa, t2 :: tsb) => if (rank(t1) < rank(t2)) t1 :: mergeTrees(tsa, ts2)
    else if (rank(t2) < rank(t1)) t2 :: mergeTrees(ts1, tsb)
    else insTree(link(t1, t2), mergeTrees(tsa, tsb))
  }

}

object SkewBinomialHeap {

  type SkewHeap[+A] = List[Node[A]]

  def apply[A <% Ordered[A]](as: A*): SkewBinomialHeap[A] = {
    var h = empty[A]
    for (a <- as) {
      h = h.insert(a)
    }
    h
  }

  def empty[A <% Ordered[A]]: SkewBinomialHeap[A] = new SkewBinomialHeap[A](Nil)

  private def root[B <% Ordered[B]](node: Node[B]): B = {
    val Node(_, value, _, _) = node
    value
  }

  private def skewLink[B <% Ordered[B]](x: B, t1: Node[B], t2: Node[B]): Node[B] = {
    val Node(r, y, ys, c) = link(t1, t2)
    if (x <= y) Node(r, x, y :: ys, c)
    else Node(r, y, x :: ys, c)
  }

  private def link[B <% Ordered[B]](t1: Node[B], t2: Node[B]): Node[B] = {
    val Node(r, x1, xs1, c1) = t1
    val Node(_, x2, xs2, c2) = t2
    if (x1 <= x2) Node(r + 1, x1, xs1, t2 :: c1)
    else Node(r + 1, x2, xs2, t1 :: c2)
  }

  private def rank[B <% Ordered[B]](node: Node[B]): Int = {
    val Node(rank, _, _, _) = node
    rank
  }

  private[utils] case class Node[+A](rank: Int, value: A, children: List[A], trees: List[Node[A]])

}


object MainApp extends App {
   var heap = SkewBinomialHeap.empty[Int]
   for( i <- 10 to 1 by -1){
     heap = heap.insert(i)
   }
   heap.findMin()
}

