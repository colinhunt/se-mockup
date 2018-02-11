import glob
import json
import re
from collections import defaultdict

class AttrDict(defaultdict):
    def __init__(self, *args, **kwargs):
        super(AttrDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


STYLE_ELEMENTS_SRC_DIR = 'elm-stuff/packages/mdgriffith/style-elements/*/src/'

FILE_PATHS = [
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element.elm')[0], 'Elem', 'Element style variation msg'),
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element/Attributes.elm')[0], 'Attr', 'Attribute variation msg'),
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element/Attributes.elm')[0], 'Lngth', 'Length'),
]

FCNNAME_FCNSIG_FCNDEF_REGEX = r"^(\w+) : (.+)\n([\w\s]+) =$"

ARG_LOOKUP = {
    'Style': { 'type': 'Style', 'var_name': 'sty' },
    'String': { 'type': 'String', 'var_name': 'str' },
    'Float': { 'type': 'Float', 'var_name': 'flt' },
    'Bool': { 'type': 'Bool', 'var_name': 'bool' },
    'Length': { 'type': 'Lngth', 'var_name': 'lng' },
    'Int': { 'type': 'Int', 'var_name': 'int' },
    'ListElementStyleVariationMsg': { 'type': '(List El)', 'var_name': 'els' },
    'ListAttributeVariationMsg': { 'type': '(List Attr)', 'var_name': 'attrs' },
    'ElementStyleVariationMsg': { 'type': 'El', 'var_name': 'el' },
    'AttributeVariationMsg': { 'type': 'Attr', 'var_name': 'attr' }

}

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

print 'module Elem exposing (..)'
print
print 'import Element exposing (..)'
print 'import Element.Attributes exposing (..)'
print 'import View.Stylesheet exposing (..)'
print
print
print '''
type alias Elid =
    Int


type alias El =
    { id : Elid, el : Elem }


type Msg
    = Msg
'''


for file_path, kind, suffix in FILE_PATHS:
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

        fcn_sig = fcn_sig.title()

        arg_names = fcn_def.split()[1:]

        types = fcn_sig.split(' -> ')

        # print fcn_sig
        # print types

        type_name_parts = [type.replace(' ', '').replace('(', '').replace(')', '') for type in types]

        # all_types.update(set(type_name_parts))

        # print type_name_parts

        arguments = [ARG_LOOKUP[part]['type'] for part in type_name_parts[:-1]]

        type_name = ''.join(type_name_parts) \
            .replace('StyleVariationMsg', '') \
            .replace('VariationMsg', '') \
            .replace('Attribute', 'Attr') \
            .replace('Element', 'Elmnt') \
            .replace('Style', 'Sty') \
            .replace('String', 'Str') \
            .replace('Float', 'Flt') \
            .replace('Length', 'Lng')


        fcn_type_alias_name = type_name + 'Fn'

        if fcn_sig not in functions:
            aDict = AttrDict()
            aDict.update({
                'sig': fcn_sig,
                'sig_types': types,
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

    print 'type', kind
    for i, fcn_type in enumerate(fcn_types):
        print '    =' if i == 0 else '    |', fcn_type.type_name, fcn_type.fcn_type_alias_name, ' '.join(fcn_type.arguments)

    print
    print

    print '{-| Case statement template: -}'
    print 'case' + kind, kind.lower(), '='
    print '    case', kind.lower(), 'of'
    for fcn_type in fcn_types:
        print '        ', fcn_type.type_name, 'f', ' '.join([ARG_LOOKUP[arg]['var_name'] + str(i) for i, arg in enumerate(fcn_type.type_name_parts[:-1])]), '->'
        print '            ', kind.lower()

    print

    for fcn_type in fcn_types:
        print 'type alias', fcn_type.fcn_type_alias_name, '=', fcn_type.sig
        print
        print


    for fcn_type in fcn_types:
        for fcn in fcn_type.members:
            fcn_name = 'new' + fcn.name.title()
            print fcn_name, ':', ' -> '.join(fcn_type.arguments + [kind])
            print ' '.join(['new' + fcn.name.title()] + fcn.arg_names + ['='])
            print '    {} {} {}'.format(fcn_type.type_name, fcn.name, ' '.join(fcn.arg_names))
            print
            print