package chatterbox

class ChatSession(bot: ChatBot, iface: ChatInterface) {

  private val QUIT_SEQ = "q"

  def start() {
    iface.system.say("Welcome to ChatterBot v0.1")
    iface.system.say(s"You are talking to " + iface.bot.label)
    iface.system.say(s"Enter $QUIT_SEQ to quit")

    var input = iface.getInput

    while (input != QUIT_SEQ) {
      bot.query(input)
      input = iface.getInput
    }

    iface.system.say("Goodbye!\n")
  }

}
