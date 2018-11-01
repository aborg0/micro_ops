package com.github.aborg0.micro_ops

import java.util.Comparator

import language.experimental.macros
import scala.reflect.macros.blackbox
//import scala.reflect.runtime.universe._


package object micro_ops {

  implicit class SortByOptimized[T](val seq: Seq[T]) extends AnyVal {
    def sortByOptX(implicit comp: Comparator[T]): Seq[T] = seq.sortBy(identity)

    // Using seq: https://stackoverflow.com/a/46005988
    def sortBySimpleEagerRT[T1: Ordering, T2: Ordering](mapping: T => (T1, T2)): Seq[T /*(T1, T2)*/ ] = macro SortByOptimizedImpl.sortBySimpleEagerRTImpl[T, T1, T2]

    //    def splitt[T1, T2](t1: T1, t2: T2): (T2, T1) = macro SortByOptimizedImpl.splittt[T1, T2]
  }

  object SortByOptimizedImpl {

    def sortBySimpleEagerRTImpl[T: c.WeakTypeTag, T1: c.WeakTypeTag, T2: c.WeakTypeTag](c: blackbox.Context)(
      mapping: c.Expr[T => (T1, T2)])(
                                                                                         t1: c.Expr[Ordering[T1]], t2: c.Expr[Ordering[T2]]): c.Expr[Seq[T /*(T1, T2)*/ ]] = {
      import c.universe._
      val orig =
        reify {
          c.prefix.splice.asInstanceOf[SortByOptimized[T]].seq
        }

      val tType = implicitly[WeakTypeTag[T]].tpe
      mapping.tree match {
        case Function(Seq(ValDef(Modifiers(_, m, _), TermName(v), TypeTree(), EmptyTree)),
        Apply(TypeApply(Select(Select(Ident(TermName("scala")), TermName("Tuple2")), TermName("apply")), args),
        Seq(v1, v2 /*Select(Ident(TermName(sameVV2)), v2)*/)))
          if /*v == sameVV1 && */
          /*v == sameVV2 && */ isSimpleSelectApplicable(c)(v, v1).isDefined && isSimpleSelectApplicable(c)(v, v2).isDefined => c.warning(c.enclosingPosition, s"$m ${m.getClass} $v, : ($args); ($v1, ${v1.getClass}), ($v2, ${v2.getClass})")
          c.warning(c.enclosingPosition, showRaw(mapping.tree))
          val Some(simpleApplicable1) = isSimpleSelectApplicable(c)(v, v1)
          val Some(simpleApplicable2) = isSimpleSelectApplicable(c)(v, v2)
          //          val negate1: c.Expr[Boolean] = c.Expr[Boolean](Literal(Constant(!simpleApplicable1)))
          //          val negate2: c.Expr[Boolean] = c.Expr[Boolean](Literal(Constant(!simpleApplicable1)))
          //          c.warning(c.enclosingPosition, simpleApplicable.toString)
          //          if (simpleApplicable) {
          //          reify {
          //            orig.splice.sorted((x: T, y: T) => {
          //
          //              val compare1 = t1.splice.compare(c.Expr[T1](transformSimple(c)("x", v1, v) /*Select(Ident(TermName("x")), v1)*/).splice, c.Expr[T1](transformSimple(c)("y", v1, v) /*Select(Ident(TermName("y")), v1)*/).splice)
          //              if (compare1 != 0) {
          //                import scala.tools.reflect.Eval
          //                compareExpr(c)(compare1, simpleApplicable1).splice
          //              } else {
          //                val compare2 = t2.splice.compare(c.Expr[T2](transformSimple(c)("x", v2, v)).splice, c.Expr[T2](transformSimple(c)("y", v2, v)).splice)
          //                if (negate2.splice) {
          //                  if (compare2 == Int.MinValue) {
          //                    1
          //                  } else -compare2
          //                } else
          //                compare2
          //              }
          //            })
          //          }

          c.Expr[Seq[T]](
            q"""
               $orig.sorted(new Ordering[$tType]{def compare(x: $tType, y: $tType): Int = {
                val compare1 = $t1.compare(..${transformSimpleSelect(c)("x", v1, v)},
              ..${transformSimpleSelect(c)("y", v1, v)})
              if (compare1 != 0) {
               if ($simpleApplicable1)
                compare1
                else if (compare1 == Int.MinValue) 1 else -compare1
                } else {
                  val compare2 = $t2.compare(..${transformSimpleSelect(c)("x", v2, v)}, ..${transformSimpleSelect(c)("y", v2, v)})
                  if ($simpleApplicable2) compare2 else if (compare2 == Int.MinValue) 1 else -compare2
                }
             }})
             """)
        //      ..${compareTree(c)(Ident(TermName("compare2")), !simpleApplicable2)}


        //          } else {
        //            reify {
        //              orig.splice.sorted((x: T, y: T) => {
        //
        //                val compare1 = t1.splice.compare(c.Expr[T1](transformSimple(c)("x", v1, v) /*Select(Ident(TermName("x")), v1)*/).splice, c.Expr[T1](transformSimple(c)("y", v1, v) /*Select(Ident(TermName("y")), v1)*/).splice)
        //                if (compare1 != 0) {
        //                  /*if (compare1 == Int.MinValue) {
        //                    1
        //                  } else*/ -compare1
        //                } else {
        //                  val compare2 = 0
        //                  compare2
        //                }
        //              })
        //            }
        //          }

        case Function(Seq(ValDef(_, TermName(anon), TypeTree(), EmptyTree)),
        Match(Ident(TermName(anonSame)),
        Seq(CaseDef(/*Bind(TermName(bind),*/
        /*Apply(/*TypeTree().setOriginal(Ident(com.github.aborg0.micro_ops.Y))*/fun, Seq(
          Bind(TermName(v1), Ident(termNames.WILDCARD)),
          Bind(TermName(v2), Ident(termNames.WILDCARD)),
          Ident(termNames.WILDCARD)))/*)*/*/ caseDefArg,
        EmptyTree /*no guard*/ ,
        body
        /*Apply(TypeApply(Select(Select(Ident(scala), scala.Tuple2), TermName("apply")), List(TypeTree(), TypeTree())), List(Select(Select(Ident(TermName("y")), TermName("n")), TermName("unary_$minus")), Ident(TermName("s"))))*/)))) if anon == anonSame => c.warning(c.enclosingPosition, s"Something:     $body")
          c.warning(c.enclosingPosition, s"binding showRaw: ${showRaw(mapping.tree)}")
          caseDefArg match {
            case Bind(TermName(bind), Apply(fun, bindArgs)) => c.warning(c.enclosingPosition, "bind found")
              body match {
                case Apply(TypeApply(Select(Select(Ident(TermName("scala")), TermName("Tuple2")), TermName("apply")),
                Seq(TypeTree(), TypeTree())), Seq(
                v1,
                v2)) if isSimpleApplicable(c)(v1).orElse(isSimpleSelectApplicable(c)(bind, v1)).isDefined && isSimpleApplicable(c)(v2).orElse(isSimpleSelectApplicable(c)(bind, v2)).isDefined =>
                  c.warning(c.enclosingPosition, s"ssssss : ${termNameOfIdentOrSelect(c)(v1, if (isSimpleApplicable(c)(v1).isDefined) None else Some(bind))}   ${termNameOfIdentOrSelect(c)(v2, Some(bind))}")
                  val Some(simpleApplicable1) = isSimpleApplicable(c)(v1).orElse(isSimpleSelectApplicable(c)(bind, v1))
                  val Some(simpleApplicable2) = isSimpleApplicable(c)(v2).orElse(isSimpleSelectApplicable(c)(bind, v2))
                  val Some(v1t) = termNameOfIdentOrSelect(c)(v1, if (isSimpleApplicable(c)(v1).isDefined) None else Some(bind))
                  val Some(v2t) = termNameOfIdentOrSelect(c)(v2, if (isSimpleApplicable(c)(v2).isDefined) None else Some(bind))
                  //                  val Ident(v1t: c.TermName) = v1
                  //                  val Ident(v2t: c.TermName) = v2
                  val x1 = c.freshName(s"x1_${v1t.encodedName}$$")
                  val x2 = c.freshName(s"x2_${v2t.encodedName}$$")
                  val y1 = c.freshName(s"y1_${v1t.encodedName}$$")
                  val y2 = c.freshName(s"y2_${v2t.encodedName}$$")
                  //              c.Expr[Seq[T]](q"$orig")
                  c.Expr[Seq[T]](
                    q"""
               $orig.sorted((x: $tType, y: $tType)=> {
               val $fun(..${renames(c)(bindArgs, IndexedSeq((x1, v1t), (x2, v2t)))}) = x
                       val $fun(..${renames(c)(bindArgs, IndexedSeq((y1, v1t), (y2, v2t)))}) = y
                val compare1 = $t1.compare(..${transformSimpleOrSelect(c)(v1t.toString, x1, v1, bind, "x")},
              ..${transformSimpleOrSelect(c)(v1t.toString, y1, v1, bind, "y")})
              if (compare1 != 0) {
               if ($simpleApplicable1)
                compare1
                else if (compare1 == Int.MinValue) 1 else -compare1
                } else {
                  val compare2 = $t2.compare(..${transformSimpleOrSelect(c)(v2t.toString, x2, v2, bind, "x")}, ..${transformSimpleOrSelect(c)(v2t.toString, y2, v2, bind, "y")})
                  if ($simpleApplicable2) compare2 else if (compare2 == Int.MinValue) 1 else -compare2
                }
             })
             """)
              }

//              c.Expr[Seq[T]](q"$orig")
            case Apply(fun, bindArgs) => c.warning(c.enclosingPosition, "simple")
              body match {
                case Apply(TypeApply(Select(Select(Ident(TermName("scala")), TermName("Tuple2")), TermName("apply")),
                Seq(TypeTree(), TypeTree())), Seq(
                v1,
                v2)) if isSimpleApplicable(c)(v1).isDefined && isSimpleApplicable(c)(v2).isDefined =>
                  val Some(simpleApplicable1) = isSimpleApplicable(c)(v1)
                  val Some(simpleApplicable2) = isSimpleApplicable(c)(v2)
                  val Some(v1t) = termNameOfIdentOrSelect(c)(v1, None)
                  val Some(v2t) = termNameOfIdentOrSelect(c)(v2, None)
                  //                  val Ident(v1t: c.TermName) = v1
                  //                  val Ident(v2t: c.TermName) = v2
                  val x1 = c.freshName(s"x1_${v1t.encodedName}$$")
                  val x2 = c.freshName(s"x2_${v2t.encodedName}$$")
                  val y1 = c.freshName(s"y1_${v1t.encodedName}$$")
                  val y2 = c.freshName(s"y2_${v2t.encodedName}$$")
                  //              c.Expr[Seq[T]](q"$orig")
                  c.Expr[Seq[T]](
                    q"""
               $orig.sorted((x: $tType, y: $tType)=> {
               val $fun(..${renames(c)(bindArgs, IndexedSeq((x1, v1t), (x2, v2t)))}) = x
                       val $fun(..${renames(c)(bindArgs, IndexedSeq((y1, v1t), (y2, v2t)))}) = y
                val compare1 = $t1.compare(..${transformSimple(c)(v1t.toString, x1, v1)},
              ..${transformSimple(c)(v1t.toString, y1, v1)})
              if (compare1 != 0) {
               if ($simpleApplicable1)
                compare1
                else if (compare1 == Int.MinValue) 1 else -compare1
                } else {
                  val compare2 = $t2.compare(..${transformSimple(c)(v2t.toString, x2, v2)}, ..${transformSimple(c)(v2t.toString, y2, v2)})
                  if ($simpleApplicable2) compare2 else if (compare2 == Int.MinValue) 1 else -compare2
                }
             })
             """)
              }
          }


        //case Function(Seq(ValDef(_, TermName(x), _, _), Match(Ident(TermName(x2)),
        //Seq(CaseDef(Bind(TermName(oo), Apply(id, Seq(
        //Bind(TermName(a), Ident(termNames.WILDCARD)),
        //Bind(TermName(b), Ident(termNames.WILDCARD)),
        //Ident(termNames.WILDCARD)))), guard, _)))
        //), body)/*q"($newValue) => $sameValue match { case $t(..$pats) => ($r1, $r2) }" if newValue == sameValue */=> c.warning(c.enclosingPosition, s"pattern match")
        //  c.warning(c.enclosingPosition, s"raw: ${showRaw(mapping.tree)}")
        //
        //  c.Expr[Seq[T]](q"$orig")
        case _ =>
          c.warning(c.enclosingPosition, showRaw(mapping.tree))
          c.warning(c.enclosingPosition, s"failed to optimize: ${mapping.tree.getClass} | ${mapping.tree.children.zipWithIndex.map { case (t, i) => (i, t, t.getClass) }.toVector}")
          reify {
            orig.splice.sortBy(mapping.splice)(Ordering.Tuple2(t1.splice, t2.splice))
          }
      }
    }

  }

  private def termNameOfIdentOrSelect(c: blackbox.Context)(tree: c.Tree, varName: Option[String]): Option[c.TermName] = {
    import c.universe._
    tree match {
      case Ident(tn: c.TermName) => Some(tn)
      case Select(Ident(tn: c.TermName), TermName("unary_$minus")) => Some(tn)
      case selector =>
        c.warning(c.enclosingPosition, s"unsupported expression: $selector")
        varName.map(v => tree match {
          case Select(Ident(TermName(`v`)), tn: c.TermName) => tn
          case Select(Select(Ident(TermName(`v`)), tn: c.TermName), TermName("unary_$minus")) => tn
        })
    }
  }

  private def renames(c: blackbox.Context)(bindingArgs: Seq[c.Tree], variables: IndexedSeq[(String, c.TermName)]): Seq[c.Tree] = {
    import c.universe._
    bindingArgs.map {
      case Bind(tn, w0@(Typed(Ident(termNames.WILDCARD), _) | Ident(termNames.WILDCARD))) if variables.exists(_._2 == tn) => Bind(TermName(variables.find(_._2 == tn).get._1), w0)
      case Bind(tn, w1@(Typed(Ident(termNames.WILDCARD), _) | Ident(termNames.WILDCARD))) => Bind(TermName(c.freshName("unues")), w1)
      case w@Ident(termNames.WILDCARD) => w
    }
  }

  private def compareTree(c: blackbox.Context)(compare: c.Tree, negate: Boolean): c.Tree = {
    import c.universe._
    //    If(negate, If())
    if (negate) {
      q"""if ($compare == Int.MinValue) {
         1
         } else -$compare"""
      //c.Expr[Int](Select(Literal(Constant(compare)), TermName("unary_$minus")))
    } else {
      Literal(Constant(compare))
    }
  }

  private def compareExpr(c: blackbox.Context)(compare: Int, negate: Boolean): c.Expr[Int] = {
    import c.universe._
    //    If(negate, If())
    if (negate) {
      c.Expr[Int](
        q"""if ($compare == Int.MinValue) {
         1
         } else -$compare""")
      //c.Expr[Int](Select(Literal(Constant(compare)), TermName("unary_$minus")))
    } else {
      c.Expr[Int](Literal(Constant(compare)))
    }
    //    q""" {
    //        if ($negate) {
    //          if ($compare == Int.MinValue) {
    //            1
    //          } else -$compare
    //        } else
    //          $compare
    //          }
    //        """
  }


  //
  //      import c.universe._
  //    def splittt[T1: c.WeakTypeTag, T2: c.WeakTypeTag](c: blackbox.Context)(t1: c.Expr[T1], t2: c.Expr[T2]): c.Expr[(T2, T1)] = {

  private def transformSimpleOrSelect(c: blackbox.Context)(nameOfLocalVariable: String, freshName: String, tree: c.Tree, nameOfTreeVariable: String, renamedVariable: String): c.Tree = {
    import c.universe._

    tree match {
      case Select(Select(Ident(TermName(`nameOfTreeVariable`)), selector), TermName("unary_$minus")) => Select(Ident(TermName(renamedVariable)), selector)
      case Select(Ident(TermName(`nameOfLocalVariable`)), TermName("unary_$minus")) => Ident(TermName(freshName))
      case Select(Ident(TermName(`nameOfTreeVariable`)), selector) =>
        Select(Ident(TermName(renamedVariable)), selector)
      case Ident(TermName(`nameOfLocalVariable`)) => Ident(TermName(freshName))
    }
  }

  private def transformSimpleSelect(c: blackbox.Context)(nameOfLocalVariable: String, tree: c.Tree, nameOfTreeVariable: String): c.Tree = {
    import c.universe._

    tree match {
      case Select(Ident(TermName(`nameOfTreeVariable`)), selector) =>
        Select(Ident(TermName(nameOfLocalVariable)), selector)
      case Select(Select(Ident(TermName(`nameOfTreeVariable`)), selector), TermName("unary_$minus")) => Select(Ident(TermName(nameOfLocalVariable)), selector)
    }
  }

  private def transformSimple(c: blackbox.Context)(nameOfTreeVariable: String, nameOfLocalVariable: String, tree: c.Tree): c.Tree = {
    import c.universe._

    tree match {
      case Select(Ident(TermName(`nameOfTreeVariable`)), TermName("unary_$minus")) => Ident(TermName(nameOfLocalVariable))
      case Ident(TermName(`nameOfTreeVariable`)) => Ident(TermName(nameOfLocalVariable))
    }
  }


  private def isSimpleSelectApplicable(c: blackbox.Context)(name: String, tree: c.Tree): Option[Boolean] = {
    import c.universe._
    tree match {
      case Select(Ident(TermName(`name`)), _) => Some(true)
      case Select(Select(Ident(TermName(`name`)), _), TermName("unary_$minus")) => Some(false)
      case _ => None
    }
  }

  private def isSimpleApplicable(c: blackbox.Context)(tree: c.Tree): Option[Boolean] = {
    import c.universe._
    tree match {
      case Ident(TermName(_)) => Some(true)
      case Select(Ident(TermName(_)), TermName("unary_$minus")) => Some(false)
      case _ => None
    }
  }

  //      reify{
  //        (t2.splice, t1.splice)
  //      }
  //    }
}
