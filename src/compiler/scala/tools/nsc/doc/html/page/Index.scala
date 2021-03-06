/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */
 
package scala.tools.nsc
package doc
package html
package page

import model._

import scala.collection._
import scala.xml._

class Index(universe: Universe) extends HtmlPage {
  
  def path = List("allclasses.html")

  def title = {
    val s = universe.settings
    ( if (!s.doctitle.isDefault) s.doctitle.value else "" ) +
    ( if (!s.docversion.isDefault) (" " + s.docversion.value) else "" ) 
  }

  def headers =
    <xml:group>
      <link href={ relativeLinkTo(List("index.css", "lib")) }  media="screen" type="text/css" rel="stylesheet"/>
		  <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("scheduler.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("index.js", "lib")} }></script>
    </xml:group>

  def body =
    <body>
      <div id="library">
        <img class='class icon' width="13" height="13" src='lib/class.png'/>
        <img class='trait icon' width="13" height="13" src='lib/trait.png'/>
        <img class='object icon' width="13" height="13" src='lib/object.png'/>
        <img class='package icon' width="13" height="13" src='lib/package.png'/>
      </div>
      <div id="browser">
        <div id="filter"></div>
        <div class="pack" id="tpl">{
          def isExcluded(dtpl: DocTemplateEntity) = {
            val qname = dtpl.qualifiedName
            ( ( qname.startsWith("scala.Tuple") || qname.startsWith("scala.Product") ||
                qname.startsWith("scala.Function") || qname.startsWith("scala.runtime.AbstractFunction")
              ) && !(
                qname == "scala.Tuple1" || qname == "scala.Tuple2" ||
                qname == "scala.Product" || qname == "scala.Product1" || qname == "scala.Product2" ||
                qname == "scala.Function" || qname == "scala.Function1" || qname == "scala.Function2" ||
                qname == "scala.runtime.AbstractFunction0" || qname == "scala.runtime.AbstractFunction1" ||
                qname == "scala.runtime.AbstractFunction2"
              )
            )
          }
          def packageElem(pack: model.Package): NodeSeq = {
            <xml:group>
              { if (!pack.isRootPackage)
                  <h3><a class="tplshow" href={ relativeLinkTo(pack) }>{ pack.qualifiedName }</a></h3>
                else NodeSeq.Empty
              }
              <ol class="templates">{
                val tpls: Map[String, Seq[DocTemplateEntity]] =
                  (pack.templates filter (t => !t.isPackage && !isExcluded(t) )) groupBy (_.name)
                 
                val placeholderSeq: NodeSeq = <div class="placeholder"></div>
                		
                def createLink(entity: DocTemplateEntity, includePlaceholder: Boolean, includeText: Boolean) = {
                	val entityType = docEntityKindToString(entity)
                	val linkContent = ( 
                		{ if (includePlaceholder) placeholderSeq else NodeSeq.Empty } 
                		++
                		{ if (includeText) <span class="tplLink">{ Text(packageQualifiedName(entity)) }</span> else NodeSeq.Empty } 
                	)
                	<a class="tplshow" href={ relativeLinkTo(entity) }><span class={ entityType }>({ Text(entityType) })</span>{ linkContent }</a>
                }
                
                for (tn <- tpls.keySet.toSeq sortBy (_.toLowerCase)) yield {
                	val entities = tpls(tn)
                	val row = (entities find (e => e.isPackage || e.isObject), entities find (e => e.isTrait || e.isClass))
                	
                	val itemContents = row match {
              			case (Some(obj), None) => createLink(obj, includePlaceholder = true, includeText = true)
              			
              			case (maybeObj, Some(template)) => 
              				val firstLink = maybeObj match {
              					case Some(obj) => createLink(obj, includePlaceholder = false, includeText = false)
              					case None => placeholderSeq
              				}
              			
              				firstLink ++ createLink(template, includePlaceholder = false, includeText = true)
	                    
              			case _ => // FIXME: this default case should not be necessary. For some reason AnyRef is not a package, object, trait, or class
              				val entry = entities.head
              				placeholderSeq ++ createLink(entry, includePlaceholder = false, includeText = true)
               		}
                		                	
                  <li title={ entities.head.qualifiedName }>{ 
                  	itemContents
                  }</li>
                }
              }</ol>
              <ol class="packages"> {
                for (sp <- pack.packages sortBy (_.name.toLowerCase)) yield
                  <li class="pack" title={ sp.qualifiedName }>{ packageElem(sp) }</li>
              }</ol>
            </xml:group>
          }
          packageElem(universe.rootPackage)
        }</div>
      </div>
    </body>


  def packageQualifiedName(ety: DocTemplateEntity): String =
    if (ety.inTemplate.isPackage) ety.name else (packageQualifiedName(ety.inTemplate) + "." + ety.name)

}
