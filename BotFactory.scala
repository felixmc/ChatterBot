package chatterbox

class BotFactory(entity: ChatEntity) {

  def Hi() = new HiBot(entity)
  def Greet() = new GreetBot(entity)
  def Regex(p: Map[String, List[String]]) = new RegexBot(entity, p)
}
