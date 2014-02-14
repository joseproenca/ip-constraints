package cf.featm

/**
 * Created with IntelliJ IDEA.
 *
 * Created by jose on 31/07/13.
 */
case class Feat(name:String,gr:Group,at:Attrs) {
  def this(name:String) = this(name,EmptyGroup,Attrs())
}

sealed class Group()
case object EmptyGroup extends Group
case class OneOf(feats:List[Feat]) extends Group
case class AllOf(feats:List[Feat]) extends Group
case class Card(from:Int,to:Int,feats:List[Feat]) extends Group

case class Attrs(p:(String,AttrRange)*)

sealed class AttrRange()
case class IntAttr(from:Int,to:Int) extends AttrRange

//case class Attr(featName:String,attrName:String)

class FeatSel() {
  private var _feats = Map[String,Boolean]() withDefaultValue false
  private var _intAttrs = Map[(String,String),Int]()
//  private var _boolAttrs = Map[(String,String),Boolean]()

  def apply(f:String): Boolean = _feats.apply(f)
  def has(f:String) = _feats.contains(f)

  def apply(f:String,a:String): Int = _intAttrs.apply(f,a)
  def has(f:String,a:String) = _intAttrs.contains(f,a)

  def update(f:String,b:Boolean) { _feats += (f -> b) }
  def update(f:String,a:String,i:Int) { _intAttrs += ((f,a) -> i) }
//  def update(fa:(String,String),b:Boolean) { _boolAttrs += (fa -> b) }


  def feat(f:String) = _feats.apply(f)
  def attr(f:String,a:String) = _intAttrs.apply(f,a)

  override def toString = {
    _feats.mkString("[",",","") ++
      (if (_feats.isEmpty) "" else ",") ++
      _intAttrs.mkString("",",","]")
  }
}


// helpers to build feature models
object Feat  {
  def apply(name:String): Feat = Feat(name,EmptyGroup,Attrs())
  def apply(name:String,g:Group): Feat = Feat(name,g,Attrs())
  def apply(name:String,a:Attrs): Feat = Feat(name,EmptyGroup,a)
}
object OneOf { def apply(feats:Feat*): OneOf = OneOf(feats.toList) }
object AllOf { def apply(feats:Feat*): AllOf = AllOf(feats.toList) }
object Card  { def apply(from:Int,to:Int,feats:Feat*): Card = Card(from,to,feats.toList) }



// Experiments

object Main extends App {
  val mergerFM =
    Feat (
      "Merger",
      AllOf(
        Feat("Simple"),
        Feat("MaxMerger") ),
      Attrs( "size" → IntAttr(1,10))
    )

  val mergerFMMM =
    Feat (
      "Merger",
      AllOf( List(
        Feat("Simple"),
        Feat("MaxMerger")) ),
      Attrs( "size" → IntAttr(1,10))
    )

  println(mergerFM)
  println(mergerFMMM == mergerFM)

  //////////

  val featSel = new FeatSel
  featSel("Merger") = true
  featSel("Merger","size") = 5
  featSel("Simple") = true

  println(featSel)
  println("merger is " + (featSel feat "Merger") + " and its size " + (featSel("Merger","size")))

  /////////

  def mergerReconf(sel:FeatSel): Int => Option[String] = {
    case conn if sel("Merger", "size") > 1 && sel("Simple") =>
      Some("mkMerger(\"a\",\"b\", "+sel("Merger","size")+")")
    case conn if sel("Merger", "size") > 1 && sel("MaxMerger") =>
      Some("mkMaxMerger(\"a\",\"b\", "+sel("Merger","size")+")")
    case _ => None
  }

  println("applying mergerReconf to our selection: "+mergerReconf(featSel)(0))

}