package chatterbox

trait ChatInterface {
  val human  : ChatEntity
  val bot    : ChatEntity
  val system : ChatEntity

  def getInput(): String
}

abstract class ChatEntity(var name: String = "Entity") {
  def say(s: String)
  def label: String
}
