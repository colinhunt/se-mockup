import glob
import json
import re
from collections import defaultdict, OrderedDict

class AttrDict(defaultdict):
    def __init__(self, *args, **kwargs):
        super(AttrDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


STYLE_ELEMENTS_SRC_DIR = 'elm-stuff/packages/mdgriffith/style-elements/*/src/'

FILE_PATHS = [
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element.elm')[0], 'Elem', 'el', 'Element style variation msg'),
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element/Attributes.elm')[0], 'Attr', '', 'Attribute variation msg'),
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element/Attributes.elm')[0], 'Lngth', '', 'Length'),
]

FCNNAME_FCNSIG_FCNDEF_REGEX = r"^(\w+) : (.+)\n([\w\s]+) =$"
TYPE_VAR_REGEX = r"\b[a-z]+\b"

PLACEHOLDER = '{ id = id + 1, name = "text", elem = StrElmnt text "Click to edit" }'

ARG_LOOKUP = {
    'Sty': { 'type': 'sty', 'var_name': 'sty', 'default': 'Sty.Elmnt' },
    'String': { 'type': 'String', 'var_name': 'str', 'default': '"placeholder"' },
    'Float': { 'type': 'Float', 'var_name': 'flt', 'default': '10' },
    'Bool': { 'type': 'Bool', 'var_name': 'bool', 'default': 'False' },
    'Length': { 'type': 'Lngth', 'var_name': 'lng', 'default': '(FltLng px 10)' },
    'Int': { 'type': 'Int', 'var_name': 'int', 'default': '10' },
    'ListElementStyVarMsg': { 'type': '(List (El sty var msg))', 'var_name': 'els', 'default': '[{}]'.format(PLACEHOLDER) },
    'ListAttributeVarMsg': { 'type': '(List (Attr var msg))', 'var_name': 'attrs', 'default': '[FltAttr padding 20]' },
    'ElementStyVarMsg': { 'type': '(El sty var msg)', 'var_name': 'el', 'default': PLACEHOLDER },
    'AttributeVarMsg': { 'type': '(Attr var msg)', 'var_name': 'attr', 'default': '(StrAttr "default")' }

}

def unique(sequence):
    seen = set()
    return [x for x in sequence if not (x in seen or seen.add(x))]

def excluded(fcn_name, fcn_sig, suffix):
    return (
        (not (fcn_sig.endswith(suffix)))
        or fcn_sig.startswith('(')
        or '-> (' in fcn_sig
        or 'grid' in fcn_sig.lower()
        or 'html' in fcn_sig.lower()
        or 'List (List' in fcn_sig
        or 'variation ->' in fcn_sig
        or fcn_name in [
            'numbered',
            'bulleted',
            'ping',
            'rel',
            'classList',
            'inlineStyle',
            'property',
            'attribute',
            'language'
        ]
    )

# def make_arg(part):

print 'module Layout.Element exposing (..)'

print 'import Element exposing (..)'
print 'import Element.Attributes exposing (..)'
print 'import View.Stylesheet as Sty'

print '''
type alias Elid =
    Int


type alias El sty var msg =
    { id : Elid, name : String, elem : Elem sty var msg }
'''


for file_path, kind, var, suffix in FILE_PATHS:
    functions = {}

    content = ""

    with open(file_path, 'r') as f:
        content = f.read()

    matches = re.finditer(FCNNAME_FCNSIG_FCNDEF_REGEX, content, re.MULTILINE)

    # print len(list(matches))
    for matchNum, match in enumerate(matches):
        fcn_name, fcn_sig, fcn_def = match.groups()

        if excluded(fcn_name, fcn_sig, suffix):
            continue



        type_vars = unique(re.findall(TYPE_VAR_REGEX, fcn_sig))
        for tv in type_vars:
            fcn_sig = fcn_sig.replace(tv, tv[:3])

        type_vars = ' '.join([tv[:3] for tv in type_vars])

        arg_names = fcn_def.split()[1:]

        types = fcn_sig.title().split(' -> ')

        # print fcn_sig
        # print types

        type_name_parts = [type.replace(' ', '').replace('(', '').replace(')', '') for type in types]

        # all_types.update(set(type_name_parts))

        # print type_name_parts

        arguments = [ARG_LOOKUP[part]['type'] for part in type_name_parts[:-1]]

        type_name = ''.join(type_name_parts) \
            .replace('StyVarMsg', '') \
            .replace('VarMsg', '') \
            .replace('Attribute', 'Attr') \
            .replace('Element', 'Elmnt') \
            .replace('String', 'Str') \
            .replace('Float', 'Flt') \
            .replace('Length', 'Lng')


        fcn_type_alias_name = type_name + 'Fn'

        if fcn_sig not in functions:
            aDict = AttrDict()
            aDict.update({
                'sig': fcn_sig,
                'sig_types': types,
                'type_vars': type_vars,
                'type_name': type_name,
                'type_name_parts': type_name_parts,
                'fcn_type_alias_name': fcn_type_alias_name,
                'arguments': arguments,
                'members': []
            })
            functions[fcn_sig] = aDict

        aDict = AttrDict()
        aDict.update({
            'name': fcn_name,
            'arg_names': arg_names,
        })
        functions[fcn_sig].members.append(aDict)

    # print json.dumps(functions, indent=4)
    # exit(0)
    fcn_types = sorted(functions.values(), cmp=lambda x,y: len(x.type_name) - len(y.type_name))

    kind_full = '{} {}'.format(kind, fcn_types[0].type_vars)

    print 'type', kind_full
    for i, fcn_type in enumerate(fcn_types):
        print '    {token} {type_name} ({fcn_type_name} {type_vars}) {args}'.format(
            token='=' if i == 0 else '|', type_name=fcn_type.type_name, type_vars=fcn_type.type_vars,
            fcn_type_name=fcn_type.fcn_type_alias_name, args=' '.join(fcn_type.arguments))

    print
    print

    print '{-| Case statement template: -}'
    print '-- case' + kind, kind.lower(), '='
    print '--     case', kind.lower(), 'of'
    for fcn_type in fcn_types:
        args = ' '.join([ARG_LOOKUP[arg]['var_name'] for arg in fcn_type.type_name_parts[:-1]])
        print '--         ', fcn_type.type_name, 'f', args, '->'
        print '--             ', fcn_type.type_name, 'f', args

    print

    for fcn_type in fcn_types:
        print 'type alias', fcn_type.fcn_type_alias_name, fcn_type.type_vars, '=', fcn_type.sig
        print
        print


    # for fcn_type in fcn_types:
    #     for fcn in fcn_type.members:
    #         fcn_name = 'new' + fcn.name.title()
    #         print fcn_name, ':', ' -> '.join(fcn_type.arguments + [kind_full])
    #         print ' '.join(['new' + fcn.name.title()] + fcn.arg_names + ['='])
    #         print '    {} {} {}'.format(fcn_type.type_name, fcn.name, ' '.join(fcn.arg_names))
    #         print
    #         print

    print 'all{}s'.format(kind.title()), 'id' '=', '['
    for i, fcn_type in enumerate(fcn_types):
        for j, fcn in enumerate(fcn_type.members):
            print '    {comma}{{ id = id, name = "{name}", elem = {fcn_type_name} {name} {defaults} }}'.format(
                name=fcn.name, fcn_type_name=fcn_type.type_name, 
                defaults=' '.join([ARG_LOOKUP[arg]['default'] for arg in fcn_type.type_name_parts[:-1]]),
                comma=',' if (i,j) != (0,0) else ''
            )
    print ']'