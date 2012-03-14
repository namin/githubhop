package models.github

import models._

object Recommender {
  val repo = new RecommenderSystem {
    type A = Repo
    type B = User
    def a_to_b(a: Repo) = Users.watching(a)
    def b_to_a(b: User) = Repos.watched(b)
  }
  val user = repo.reverse
}