// This is program is written by Yupeng Lu and Lochlain Lewis

import scala.util.Random
import scala.collection.immutable.Vector

object BridgeGame {
  var trumpSuit: String = _  // assign value to trumpSuit later in main()
  
  def main(args: Array[String]) {
    var north = new Player("North")
    var east = new Player("East")
    var south = new Player("South")
    var west = new Player("West")
    val suitNames = List("Spades", "Hearts", "Clubs", "Diamonds", "No Trump")
    north.deal(List(north, east, south, west))                     // deal  
    trumpSuit = suitNames(Random.nextInt(5))
    var thisPlayer: Player = Random.shuffle(List(north, east, south, west)).head
    var nextPlayer: Player = null
    var suitPlaying: String = "lead"
    var winner: Player = thisPlayer
       
    /**
     * Print out the trump suit
     */
    println()
    println("trump suit is " + trumpSuit)
    println()
    
    /**
     * This loop is the playing part.
     * Each game contains 13 tricks.
     * Using two temporary variables of type Player: thisPlayer and nextPlayer
     */
    for (i <- 1 to 13) {
      var numberOfPlayedPlayers = 0
      println()
      println("Trick # " + i)
      thisPlayer = winner
      suitPlaying = (Random.shuffle(thisPlayer.hand.cards)).head.suit
      while (numberOfPlayedPlayers <= 3) {
        if (numberOfPlayedPlayers == 0) {
          thisPlayer.play(suitPlaying)
          suitPlaying = thisPlayer.cardPlay.suit
        }
        
        /**
         * Calling method next to decide who is the following player.
         */
        nextPlayer = next(thisPlayer)
        if (numberOfPlayedPlayers != 3) {
          nextPlayer.play(suitPlaying)
        }
        
        winner = playerWins(winner, thisPlayer)
        
        if (numberOfPlayedPlayers == 0) {
          println(thisPlayer.name + " leads with the " + thisPlayer.cardPlay)
        } else {
          println(thisPlayer.name + " plays the " + thisPlayer.cardPlay)
          // println(". His hands is " + thisPlayer.hand.cards)
        }
        numberOfPlayedPlayers += 1
        thisPlayer = nextPlayer
      }
      println("  " + winner.name + " takes the trick.")
    }
    
    /**
     * method to test who is the winner
     */  
    def playerWins(a: Player, b: Player): Player = {
      var winner: Player = null
      if (a.grade > b.grade) {
        winner = a
      } else if (a.grade < b.grade) {
        winner = b
      } else if (a.cardPlay.value == 1) {
        winner = a
      } else if (b.cardPlay.value == 1) {
        winner = b
      } else if (a.cardPlay.value > b.cardPlay.value) {
        winner = a
      } else if (a.cardPlay.value < b.cardPlay.value) {
        winner = b
      } else {
        winner = a
      }
      winner
    } 
    
    def next(player: Player): Player = {
      def nextPlayer: Player = player.name match {
        case "North" => east
        case "East" => south
        case "South" => west
        case "West" => north
      }
      nextPlayer
    }
   
  }
  
}

/**
 * Describes a single card in a standard Bridge deck.
 * @param value An integer from 1 (Ace) to 13 (King)
 * @param suit One of "Clubs", "Diamonds", "Hearts", "Spades"
 */
class Card(val value: Int, val suit: String) {
  /**
   * Returns a string that describes this Card.
   */
  override def toString =
    (value match {
      case 1 => "Ace"
      case 11 => "Jack"
      case 12 => "Queen"
      case 13 => "King"
      case _ => value.toString
    }) + " of " + suit
    
    def toShortString =
      (value match {
      case 1 => "A"
      case 11 => "J"
      case 12 => "Q"
      case 13 => "K"
      case _ => value.toString
    }) + suit(0)
}

/**
 * Describes a complete deck of 52 playing cards.
 */
object CardDeck {
  val cards = for (
    value <- 1 to 13;
    suit <- List("Clubs", "Diamonds", "Hearts", "Spades")
  ) yield new Card(value, suit)
}

/**
 * Describes a hand.
 * A hand is a set of object of class Card.
 */
class Hand {
  var cards: Set[Card] = Set()
  def add(card: Card): Unit = {
    cards += card
  }
  def remove(card: Card): Unit = {
    cards -= card
  }
  def select(suit: String): Option[Card] = {
    var cardSameSuit = cards.filter(_.suit == suit)
    if (cardSameSuit.isEmpty == false) {
      Some(cardSameSuit.head)
    } else {
      None
    }
  }
}
      
/**
 * Describes a player.
 * Value "hand" holds 13 cards at the beginning of the game.
 * cardPlay is a temporary variable of the card playing in a round.
 */
class Player(val name: String) {
  val hand = new Hand
  var cardPlay: Card = _
  var suitPlay: String = _
 // Using grade to indicate whether it's follow, trump, or pass
  def grade: Int = {      
    if (cardPlay.suit == BridgeGame.trumpSuit) {
      2
    } else if (cardPlay.suit == suitPlay) {
      1
    } else {
      0
    }
  }
  
  def deal(players: List[Player]) {
    val cardDeck = Random.shuffle(CardDeck.cards)
    var cardNumber = 0
    for (playerNumber <- 0 to 3) {
      for (n <- cardNumber to (cardNumber + 12)) {
        players(playerNumber).take(cardDeck(n))
        cardNumber = n + 1
        }
    }
  }
  
  def take(card: Card): Unit = {
    hand.add(card)
  }
  
  def play(suitLed: String): Card = {
    var cantFollow: Boolean = false
    suitPlay = suitLed
    if (suitLed == "lead") {
      cardPlay = Random.shuffle(hand.cards).head
    } else {
      hand.select(suitLed) match {
        case Some(card) => cardPlay = card
        case None => cantFollow = true
      }
    }
    
    if (cantFollow == true) {
      hand.select(BridgeGame.trumpSuit) match {
        case Some(card) => cardPlay = card
        case None => cardPlay = (Random.shuffle(hand.cards)).head
      }
    }
    hand.remove(cardPlay)
    cardPlay
  }
}
