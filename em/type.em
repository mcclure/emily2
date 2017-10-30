# Types

profile experimental

from project.util import *
from project.core import *

export Type = inherit Object
	method resolve = this
	toString = "[MYSTERY TYPE]"

# TODO: Not like this
let typeDebug = function(to, frm, why)
	print
		why + " type of ", to.loc.toString, " to ", frm.loc.toString
		if (frm)
			" (" + frm.type.toString + ")"
		else
			""
		ln

export TypedNode = inherit Node
	method unify = function(node)
		if (this.type && node.type)
			if (!this.type.compatible (node.type))
				fail
					nullJoin array
						"Type error between ", this.type, " (", this.loc
						") and ", node.type, " (", node.loc, ")"
		elif (this.type)
			this.type = this.type.resolve
			node.type = this.type
		elif (node.type)
			#typeDebug this node "Setting"
			node.type = node.type.resolve
			this.type = node.type
		else
			#typeDebug this node "Forwarding"
			this.type = new ReferType(node)

	method toString = "[Node " + this.loc.toString + " type " + (if (this.type) (this.type.toString) else ("{Unresolved}")) + "]"

export ReferType = inherit Type
	field to = null
	method resolve = if (this.to.type) (this.to.type.resolve) else (this)

	method toString = if (this.to.type)
		this.to.type.toString
	else
		"{Unresolved type at " + this.to.loc.toString + "}"

export ResolvedType = inherit Type
	method compatible = function (type) (this.compatibleTest type || type.compatibleTest this)
	method compatibleTest = function (type) (type == this)
	method returnFor = function (typedNode) (standardUnknowableVal)

export UnknowableType = inherit ResolvedType
	method compatibleTest = function (type) (true) # FIXME
	toString = "{Any}"

export InvalidType = inherit ResolvedType
	method compatibleTest = function (type) (false)
	toString = "{INTERNAL ERROR}"

export UnitType = inherit ResolvedType
	toString = "{Unit}"

export BooleanType = inherit ResolvedType
	toString = "{Boolean}"

export NumberType = inherit ResolvedType
	toString = "{Float}"

export AtomType = inherit ResolvedType
	toString = "{Atom}"

export StringType = inherit ResolvedType
	toString = "{String}"

export FunctionType = inherit ResolvedType
	field arg = null   # These are both TypedNodes
	field result = null
	method toString = nullJoin array
		"{Function ", this.arg.type, " -> ", this.result.type, "}"
	method compatibleTest = function (type)
		is FunctionType type && this.arg.type.resolve.compatibleTest (type.arg.type.resolve) && this.result.type.resolve.compatibleTest (type.result.type.resolve)
	method returnFor = function(typedNode) (this.result) # TODO: Check correctness of typedNode?

export Val = inherit TypedNode
	field type = null

export KnownTypeVal = inherit Val # Use for typed literals with no other content

let standardUnknowableVal = new KnownTypeVal(null, UnknowableType)

export functionType = foldr InvalidType function(prev, next)
	new FunctionType
		new KnownTypeVal(type=prev)
		new KnownTypeVal(type=next)

export functionTypeVal = function(a)
	new KnownTypeVal
		type = functionType a