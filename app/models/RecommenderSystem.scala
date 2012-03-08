package models

trait RecommenderSystem { self =>
  type A <: { val id : String }
  type B <: { val id : String }
  def a_to_b(a: A): Seq[B]
  def b_to_a(b: B): Seq[A]

  def similar(n: Int)(a: A): Seq[A] = {
    val b_s = a_to_b(a: A).map(b => (b.id,b)).toMap
    val a_s = b_s.values.flatMap(b => b_to_a(b).map(a => (a.id,a)).toMap).toMap
    a_s.values.toSeq.sortBy(a => a_to_b(a).count(b => b_s.contains(b.id))).take(n)
  }
  
  def reverse = new RecommenderSystem {
    type A = self.B
    type B = self.A
    def a_to_b(a: A) = self.b_to_a(a)
    def b_to_a(b: B) = self.a_to_b(b)
  }
}