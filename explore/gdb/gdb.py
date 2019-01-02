#
# gdb is using python3.5
#


import gdb

class inspector_for_type(object):
    __type_inspectors = {}

    def __init__(self, type_code):
        self._type_code = type_code

    def __call__(self, func):
        self.__type_inspectors[self._type_code] = func
        return func

    @classmethod
    def get_inspector(cls, type_code):
        return cls.__type_inspectors.get(type_code)

class TypeInspector(object):

    def __init__(self, max_depth=64):
        self._names_stack = []
        self._max_depth = max_depth;
        self._current_depth = 0;
        self._resolved_types = {}

    def resolve(self, name, type_info, context=None):
        if self.resolved(name):
            return
        if context is None:
            context = self._resolved_types
        context[name] = type_info


    def resolved(self, name):
        if name not in self._resolved_types:
            return False
        ref = self._resolved_types[name].get("ref")
        if isinstance(ref, gdb.Type):
            return False
        fields = self._resolved_types[name].get("fields", [])
        for field in fields:
            if isinstance(field['type'], gdb.Type):
                return False
        return True


    def _raze_once(self):
        context = {}
        for name, type_info in self._resolved_types.items():
            ref = type_info.get("ref")
            if isinstance(ref, gdb.Type):
                ref_info = self._inspect(ref, context)
                type_info["ref"] = ref_info["name"]
            fields = type_info.get("fields", [])
            for field in fields:
                if not isinstance(field['type'], gdb.Type):
                    continue
                finfo = self._inspect(field['type'], context)
                field["type"] = finfo["name"]
        self._resolved_types.update(context)


    def _raze_all(self):
        for depth in range(self._max_depth):
            self._raze_once()

    def _inspect(self, typ, context=None):
        if not isinstance(typ, gdb.Type):
            print(typ, typ.__class__)
            typ = gdb.lookup_type(str(typ))
        type_code = typ.code
        inspector = inspector_for_type.get_inspector(type_code)
        if inspector:
            ret = inspector(self, typ)
        else:
            ret = {"name": "{}".format(typ), "type": "atom", "ref": None, "fields": []}

        self.resolve(ret["name"], ret, context)
        return ret

    def inspect(self, typ):
        self._inspect(typ)
        self._raze_all()


@inspector_for_type(gdb.TYPE_CODE_TYPEDEF)
def ptr_inspector(inspector, typ):
    ret = {
        "name": typ.name,
        "type": "typedef",
        "ref": typ.strip_typedefs(),
        "fields": []
    }
    return ret


@inspector_for_type(gdb.TYPE_CODE_PTR)
def ptr_inspector(inspector, typ):
    target = typ.target()
    target_info = inspector._inspect(target, {})
    name = "{} *".format(target_info["name"])
    ret = {
        "name": name,
        "type": "pointer",
        "ref": target,
        "fields": []
    }
    return ret


@inspector_for_type(gdb.TYPE_CODE_ARRAY)
def array_inspector(inspector, typ):
    target = typ.target()
    target_info = inspector._inspect(target, {})
    name = "{}[{}]".format(target_info["name"], typ.range()[1])
    ret = {
        "name": name,
        "type": "array",
        "ref": target,
        "fields": []
    }
    return ret

@inspector_for_type(gdb.TYPE_CODE_STRUCT)
def ptr_inspector(inspector, typ):
    struct_name = typ.tag
    if not struct_name: # anonymous struct
        struct_name = "<struct anon {}>".format(id(typ))
    name = "struct {}".format(struct_name)
    ret = {
        "name": name,
        "type": "struct",
        "ref": None,
        "fields": [{
            "name": field.name or "<anon>",
            "type": field.type,
            "index": idx
        } for idx, field in enumerate(typ.fields())]
    }
    return ret
