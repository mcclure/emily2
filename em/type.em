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
					"Type error between " + this.loc.toString + " and " + node.loc.toString
		elif (this.type)
			#typeDebug node this "Setting"
			this.type = this.type.resolve
			node.type = this.type
		elif (node.type)
			#typeDebug this node "Setting"
			node.type = node.type.resolve
			this.type = node.type
		else
			#typeDebug this node "Forwarding"
			this.type = new ReferType(node)

export ReferType = inherit Type
	field to = null
	method resolve = if (this.to.type) (this.to.type.resolve) else (this)

	method toString = if (this.to.type)
		to.type.toString
	else
		"{Unresolved type at " + this.to.loc.toString + "}"

export ResolvedType = inherit Type
	method compatible = function (type) (this.compatibleTest type || type.compatibleTest this)
	method compatibleTest = function (type) (type == this)
	method returnType = function (typedNode) (UnknowbleType)

export UnknowableType = inherit ResolvedType
	method compatibleTest = function (type) (true) # FIXME
	toString = "{Any}"

export InvalidType = inherit ResolvedType
	method compatibleTest = function (type) (false)
	toString = "{INTERNAL ERROR}"

export UnitType = inherit ResolvedType
	toString = "{Unit}"

export BoolType = inherit ResolvedType
	toString = "{Bool}"

export NumberType = inherit ResolvedType
	toString = "{Float}"

export AtomType = inherit ResolvedType
	toString = "{Atom}"

export StringType = inherit ResolvedType
	toString = "{String}"

export FunctionType = inherit ResolvedType
	field fn = null   # These are both TypedNodes
	field arg = null
	toString = "{Function}"
	method compatibleTest = function (type)
		is FunctionType type && this.arg.compatibleTest (type.arg) && this.result.compatibleTest (type.result)
	method returnType = arg.type

export Val = inherit TypedNode
	field type = null

export KnownTypeVal = inherit Val # Use for typed literals with no other content

export functionType = foldl InvalidType function(prev, next)
	new FunctionType
		new KnownTypeVal(type=prev)
		new KnownTypeVal(type=next)

export functionTypeVal = function(a)
	new KnownTypeVal
		type = functionType a