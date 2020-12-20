object Seven {

  case class Bag(adjective: String, color: String)
  type Rule = List[(Int, Bag)]

/*
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
 */
  val examples: Map[Bag, Rule] = Map(
    Bag("light", "red") -> List(1 -> Bag("bright", "white"), 2 -> Bag("muted", "yellow")),
    Bag("dark", "orange") -> List(3 -> Bag("bright", "white"), 4 -> Bag("muted", "yellow")),
    Bag("bright", "white") -> List(1 -> Bag("shiny", "gold")),
    Bag("muted", "yellow") -> List(2 -> Bag("shiny", "gold"), 9 -> Bag("faded", "blue")),
    Bag("shiny", "gold") -> List(1 -> Bag("dark", "olive"), 2 -> Bag("vibrant", "plum")),
    Bag("dark", "olive") -> List(3 -> Bag("faded", "blue"), 4 -> Bag("dotted", "black")),
    Bag("vibrant", "plum") -> List(5 -> Bag("faded", "blue"), 6 -> Bag("dotted", "black")),
    Bag("faded", "blue") -> Nil,
    Bag("dotted", "black") -> Nil
  )

  def findDirect(bag: Bag, rules: Map[Bag, Rule]): List[Bag] =
    rules.flatMap {
      case (container, Nil) => Nil
      case (container, bags) =>
        if (bags.exists { case (i, b) => b == bag }) List(container)
        else Nil
    }.toList

  @annotation.tailrec
  def findTransitive(
    bag: Bag,
    rules: Map[Bag, Rule],
    found: Set[Bag] = Set.empty,
    toFind: List[Bag] = List.empty): Set[Bag] = {
    val direct = findDirect(bag, rules).filterNot(found.contains)
    if (direct.isEmpty)
      if (toFind.isEmpty) found
      else findTransitive(toFind.head, rules, found, toFind.tail)
    else findTransitive(direct.head, rules, found ++ direct, toFind ++ direct.tail)
  }

  def count(bag: Bag, rules: Map[Bag, Rule]): Int =
    rules.get(bag).toList.flatten.map(_._1).sum
  
  def countDeep(bag: Bag, rules: Map[Bag, Rule], multiplier: Int = 1): Int = {
    val rest = rules.get(bag).toList.flatten
    count(bag, rules) * multiplier + rest.map { case (count, bag) =>
      countDeep(bag, rules, multiplier * count)
    }.sum
  }

  def parseRules(input: String) = input.split(",").filterNot(_.isEmpty).toList.map { phrase =>
    try {
      val count = phrase.trim.takeWhile(_.isDigit)
      val bag = phrase.trim.dropWhile(_.isDigit).trim

      (count.toInt, parseBag(bag))
    } catch {
      case e: Throwable => throw new IllegalArgumentException(phrase, e)
    }
  }

  def parseBag(input: String) = input.split(" ").toList match {
    case List(adj, color, "bag") => Bag(adj, color)
    case List(adj, color, "bag.") => Bag(adj, color)
    case List(adj, color, "bags") => Bag(adj, color)
    case List(adj, color, "bags.") => Bag(adj, color)
    case _ => throw new IllegalArgumentException(input)
  }

  def parse(input: Iterator[String]): Map[Bag, Rule] =
    input.filterNot(_.isEmpty).map { line =>
      line.trim.split(" contain ").toList match {
        case List(b, "no other bags.") => parseBag(b) -> Nil
        case List(b, rules) => parseBag(b) -> parseRules(rules)
        case _ => throw new IllegalArgumentException(line)
      }
    }.toMap

  val input = parse(io.Source.fromFile("./input/7").getLines())

  def starOne =
    findTransitive(Bag("shiny", "gold"), input).size

  def starTwo =
    countDeep(Bag("shiny", "gold"), input)

  val testDirect = require(
    Set(Bag("bright", "white"), Bag("muted", "yellow")) ==
      findDirect(Bag("shiny", "gold"), examples).toSet, "Direct example")

  val testTransitive = require(
    Set(Bag("bright", "white"), Bag("muted", "yellow"), Bag("dark", "orange"), Bag("light", "red")) ==
      findTransitive(Bag("shiny", "gold"), examples), "Transitive example")
  
  val testParse = require(examples == parse(
      """light red bags contain 1 bright white bag, 2 muted yellow bags.
      dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      bright white bags contain 1 shiny gold bag.
      muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      faded blue bags contain no other bags.
      dotted black bags contain no other bags.""".split("\n").iterator), "parse examples")

  val testExamples21 = require(32 == countDeep(Bag("shiny", "gold"), examples),
    "example for second star")
  val testExamples22 = require(
    126 == countDeep(Bag("shiny", "gold"), parse(
      """shiny gold bags contain 2 dark red bags.
      dark red bags contain 2 dark orange bags.
      dark orange bags contain 2 dark yellow bags.
      dark yellow bags contain 2 dark green bags.
      dark green bags contain 2 dark blue bags.
      dark blue bags contain 2 dark violet bags.
      dark violet bags contain no other bags.""".split("\n").iterator)),
    "second example for second star")

  def main(args: Array[String]) = {
    println(s"Answer 1: $starOne")
    println(s"Answer 2: $starTwo")
  }
}
