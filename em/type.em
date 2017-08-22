# Types

profile experimental

from project.util import *
from project.core import *

export Type = inherit Object
	method resolve = this
	toString = "[MYSTERY TYPE]"

export TypedNode = inherit Node
	method unify = function(node)
		if (this.type && node.type)
			if (!this.type.compatible (node.type))
				fail
					"Type error between " + this.loc.toString + " and " + node.loc.toString
		elif (this.type)
			this.type = this.type.resolve
			node.type = this.type
		elif (node.type)
			node.type = node.type.resolve
			this.type = node.type
		else
			this.type = new ReferType(node)

export ReferType = inherit Type
	field to = null
	method resolve = if (to.type) (to.type.resolve) else (this)

	method toString = if (to.type)
		to.type.toString
	else
		return "{Unresolved type at " + to.loc.toString + "}"

export ResolvedType = inherit Type
	method compatible = function (type) (this.compatibleTest type || type.compatibleTest this)
	method compatibleTest = function (type) (type == this)
	method returnType = function (in) (UnknowbleType)

export UnknowableType = inherit ResolvedType
	method compatibleTest = function (type) (true) # FIXME

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
	toString = "{Function}"
