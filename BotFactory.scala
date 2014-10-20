package chatterbox

class BotFactory(entity: ChatEntity) {

  def Hi() = new HiBot(entity)
  def Greet() = new GreetBot(entity)
  def Regex() = new RegexBot(entity)
}
