package com.rockthejvm.jPractice

import scala.annotation.tailrec

object TuplesMapsExJ {

  def addPerson(network: Map[String, Set[String]], name: String): Map[String, Set[String]] = {
    if (network.contains(name)) network
    else network + {name -> Set()}
  }
  def removePerson(network: Map[String, Set[String]], name: String): Map[String, Set[String]] = {
    if (network.contains(name)) {
      val networkWithout = network - name
      networkWithout.map(p => {p._1 -> {
        if (p._2.contains(name)) p._2 - name
        else p._2
      }})
    }
    else network
  }

  def addFriendship(network: Map[String, Set[String]], name1: String, name2: String): Map[String, Set[String]] = {
    if (network.contains(name1) && network.contains(name2)) {
      val name1friends = network(name1) + name2
      val networkPlus = network + {name1 -> name1friends}
      val name2friends = network(name2) + name1
      networkPlus + {name2 -> name2friends}
    }
    else network
  }
  def removeFriendship(network: Map[String, Set[String]], name1: String, name2: String): Map[String, Set[String]] = {
    if (network.contains(name1) && network.contains(name2)) {
      val name1friends = network(name1) - name2
      val networkPlus = network + {
        name1 -> name1friends
      }
      val name2friends = network(name2) - name1
      networkPlus + {
        name2 -> name2friends
      }
    }
    else network
  }

  def friendCount(network: Map[String, Set[String]], name: String): Int = {
    if (network.contains(name)) network(name).size
    else -1
  }
  def mostFriends(network: Map[String, Set[String]]): List[String] = {
    val maxFriends = network.map(pair => pair._2.size).toList.sorted.reverse.head
    network.filter(p => p._2.size == maxFriends).keys.toList
  }
  def noFriendCount(network: Map[String, Set[String]]): Int = {
    network.count(pair => pair._2.isEmpty)
  }

  def areConnected(network: Map[String, Set[String]], sourceName: String, finalName: String): Boolean = {
    @tailrec
    def slowDelete(subnet: Map[String, Set[String]], connected: Set[String]): Boolean = {
      if (connected.isEmpty) false
      else if (connected.contains(finalName)) true
      else {
        val friend = connected.head
        slowDelete(removePerson(subnet, friend), connected - friend ++ subnet(friend))
      }
    }
    slowDelete(removePerson(network, sourceName), network(sourceName).toSet)
  }

  def main(args: Array[String]): Unit = {
    val network: Map[String, Set[String]] = Map("Tom" -> Set(), "Claire" -> Set(), "Jessie" -> Set())
    val network1 = addFriendship(network, "Tom", "Claire")
    val network2 = addFriendship(network1, "Tom", "Jessie")
    val network3 = addPerson(network2, "Jane")
    val network4 = addFriendship(addPerson(network3, "Kenny"), "Kenny", "Jane")

    println(network3)
    println(removePerson(network3, "Claire"))
    println(removeFriendship(network3, "Tom", "Jessie"))

    println(friendCount(network3, "Tom"))
    println(mostFriends(network3))
    println(mostFriends(network))
    println(noFriendCount(network3))

    println(areConnected(network3, "Claire", "Jessie"))
    println(areConnected(network3, "Claire", "Jane"))
    println(areConnected(network4, "Claire", "Jane"))
    println(areConnected(addFriendship(network4, "Kenny", "Jessie"), "Claire", "Jane"))

  }
}
