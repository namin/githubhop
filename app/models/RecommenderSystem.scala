package models

import play.api.libs.concurrent.Promise

trait RecommenderSystem { self =>
  type A <: { val id : String }
  type B <: { val id : String }
  def a_to_b(a: A): Promise[Seq[B]]
  def b_to_a(b: B): Promise[Seq[A]]

  def similar(n: Int)(a: A): Promise[Seq[A]] = {
    a_to_b(a).flatMap{ a2b =>
    	val b_s = a2b.map(b => (b.id,b)).toMap
    	Promise.sequence(b_s.values.toSeq.map(b => b_to_a(b).map(_.map(a => (a.id, a))))).map(_.flatten.toMap).flatMap{ a_s =>
    	  val a_seq = a_s.values.toIndexedSeq
    	  Promise.sequence(a_seq.map(a => a_to_b(a))).map{b_seq_r =>
    	    val b_seq = b_seq_r.toIndexedSeq
    	    (0 until a_seq.length).toSeq.sortBy(i => -(b_seq(i)).count(b => b_s.contains(b.id))).take(n).map(a_seq)
    	  }
    	}
     }
  }
  
  def reverse = new RecommenderSystem {
    type A = self.B
    type B = self.A
    def a_to_b(a: A) = self.b_to_a(a)
    def b_to_a(b: B) = self.a_to_b(b)
  }
}