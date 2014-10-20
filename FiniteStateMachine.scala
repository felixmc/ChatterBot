package chatterbox

abstract class FiniteStateMachine {

  val startState: State

  def process(s: String): State = {
    var state = startState

    for (c <- s) {
      state = state.transition(c)
    }

    return state
  }

}

class State(val transitions: scala.collection.mutable.Map[Char, State], val noTransition: State, val isAccepted: Boolean = false, val isBad: Boolean = false) {
  def transition(c: Char) = transitions.getOrElse(c, noTransition)
}
