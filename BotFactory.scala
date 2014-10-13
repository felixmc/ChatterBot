package chatterbox

class BotFactory(in: BotReader, out: BotWriter) {

  def HiBot = new HiBot(in, out)
  def GreetingBot = new GreetingBot(in, out)

}
