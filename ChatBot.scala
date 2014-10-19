package chatterbox

abstract class ChatBot(entity: ChatEntity) {
  entity.name = name

  def name: String = this.getClass.getSimpleName
  def query(in: String)
}
