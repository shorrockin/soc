import sbt._

class SocProject(info:ProjectInfo) extends DefaultProject(info) {
  val shorrockin = "Shorrockin Repository" at "http://maven.shorrockin.com"
}
