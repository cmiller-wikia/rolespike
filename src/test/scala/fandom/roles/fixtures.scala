package fandom
package roles

object fixtures {
  val defaultGrants = List(
    Grant(UserId("bob"), Role("staff", Scope("global"))),
    Grant(UserId("bob"), Role("discussions-moderator", Scope("wiki:831"))),
    Grant(UserId("bob"), Role("discussions-helper", Scope("wiki:832"))),
    Grant(UserId("harold"), Role("discussions-helper", Scope("wiki:831")))
  )
}
