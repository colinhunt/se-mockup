import glob
import re
from collections import defaultdict, OrderedDict

class AttrDict(defaultdict):
    def __init__(self, *args, **kwargs):
        super(AttrDict, self).__init__(*args, **kwargs)
        self.__dict__ = self


STYLE_ELEMENTS_SRC_DIR = 'elm-stuff/packages/mdgriffith/style-elements/*/src/'

FILE_PATHS = [
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element.elm')[0], 'Elem', 'El', ('id', 'Int'), 'Element style variation msg'),
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element/Attributes.elm')[0], 'Attr', 'At', (), 'Attribute variation msg'),
    (glob.glob(STYLE_ELEMENTS_SRC_DIR + 'Element/Attributes.elm')[0], 'Lngth', 'Ln', (), 'Length'),
]

FCNNAME_FCNSIG_FCNDEF_REGEX = r"^(\w+) : (.+)\n([\w\s]+) =$"
TYPE_VAR_REGEX = r"\b[a-z]+\b"

def placeholder(i):
    return '{{ id = id + {}, name = "text", elem = StrElmnt text "placeholder" }}'.format(i)

ARG_LOOKUP = {
    'Sty': { 
        'type': 'sty', 
        'var_name': 'sty', 
        'default': 'Sty.None',
        'encoder': '(always <| Encode.string "None")',
        'decoder': '<| Decode.succeed Sty.None'
    },
    'String': { 
        'type': 'String', 
        'var_name': 'str', 
        'default': '"placeholder"',
        'encoder': 'Encode.string',
        'decoder': 'Decode.string',
    },
    'Float': {
        'type': 'Float', 
        'var_name': 'flt', 
        'default': '10',
        'encoder': 'Encode.float',
        'decoder': 'Decode.float',
    },
    'Bool': { 
        'type': 'Bool', 
        'var_name': 'bool', 
        'default': 'False',
        'encoder': 'Encode.bool',
        'decoder': 'Decode.bool',
    },
    'Length': { 
        'type': 'Ln', 
        'var_name': 'lng', 
        'default': '{ name = "px", lngth = FltLng px 10 }',
        'encoder': 'lnEncoder',
        'decoder': 'lnDecoder',
    },
    'Int': { 
        'type': 'Int', 
        'var_name': 'int', 
        'default': '10',
        'encoder': 'Encode.int',
        'decoder': 'Decode.int',
    },
    'ListElementStyVarMsg': { 
        'type': '(List (El sty var msg))', 
        'var_name': 'els', 
        'default': 'children',
        'encoder': 'Encode.list <| List.map elEncoder',
        'decoder': '<| Decode.list <| Decode.lazy (\_ -> elDecoder)',
    },
    'ListAttributeVarMsg': { 
        'type': '(List (At var msg))', 
        'var_name': 'attrs', 
        'default': '[{ name = "padding", attr = FltAttr padding 20}]',
        'encoder': 'Encode.list <| List.map atEncoder',
        'decoder': '<| Decode.list atDecoder',
    },
    'ElementStyVarMsg': { 
        'type': '(El sty var msg)', 
        'var_name': 'el', 
        'default': '(children |> List.head |> Maybe.withDefault {id = id + 1, name = "empty", elem = Elmnt empty})',
        'encoder': 'elEncoder',
        'decoder': '<| Decode.lazy (\_ -> elDecoder)',
    },
    'AttributeVarMsg': { 
        'type': '(At var msg)', 
        'var_name': 'attr', 
        'default': '(StrAttr "default")',
        'encoder': 'atEncoder',
        'decoder': 'atDecoder',
    }

}

def unique(sequence):
    seen = set()
    return [x for x in sequence if not (x in seen or seen.add(x))]

def make_unique(strings):
    seen = []
    unique = []
    for string in strings:
        count = seen.count(string)
        if count:
            unique.append(string + str(count))
        else:
            unique.append(string)
        seen.append(string)

    return unique


def fn_lookup_name(fcn_type_alias_name):
    return fcn_type_alias_name[:1].lower() + fcn_type_alias_name[1:] + 'Lookup'


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

def make_defaults(types):
    return [ARG_LOOKUP[type]['default'] for type in types]


print 'module Layout.Element exposing (..)'

print 'import Element exposing (..)'
print 'import Element.Attributes exposing (..)'
print 'import View.Stylesheet as Sty'
print 'import Dict exposing (Dict)'
print 'import Json.Encode as Encode'
print 'import Json.Decode as Decode'
print 'import Utils exposing ((=>))'
print 'import Layout.Utils exposing (fnDecoder)'

print '''
type alias Elid =
    Int


type alias El sty var msg =
    { id : Elid, name : String, elem : Elem sty var msg }

type alias At var msg =
    { name : String, attr: Attr var msg }

type alias Ln =
    { name : String, lngth: Lngth }
'''


for file_path, kind, rec_name, extra_rec_field, suffix in FILE_PATHS:
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


    if extra_rec_field:
        extra_field_name = extra_rec_field[0]
        extra_field_type = extra_rec_field[1] + '->'
        extra_field_assign = '{} = {},'.format(extra_field_name, extra_field_name)
        extra_field_encode = '"{0}" => Encode.{1} thing.{0},'.format(extra_field_name, extra_rec_field[1].lower())
        extra_field_decode = '(Decode.field "{0}" Decode.{1})'.format(extra_field_name, extra_rec_field[1].lower())
    else:
        extra_field_name = ''
        extra_field_type = ''
        extra_field_assign = ''
        extra_field_encode = ''
        extra_field_decode = ''
    if kind == 'Elem':
        extra_arg_2 = 'children'
        extra_type_2 = 'List (El Sty.Style var msg) ->'
    else:
        extra_arg_2 = ''
        extra_type_2 = ''

    print 'all{}s'.format(kind.title()), ':', extra_field_type, extra_type_2, 'List ({rec_name} {type_vars})'.format(rec_name=rec_name, type_vars=' '.join(['Sty.Style' if var == 'sty' else var for var in fcn_type.type_vars.split()]))
    print 'all{}s'.format(kind.title()), extra_field_name, extra_arg_2, '=', '['
    for i, fcn_type in enumerate(fcn_types):
        for j, fcn in enumerate(fcn_type.members):
            print '    {comma}{{ {extra_field} name = "{name}", {kind} = {fcn_type_name} {name} {defaults} }}'.format(
                name=fcn.name, kind=kind.lower(), fcn_type_name=fcn_type.type_name, 
                defaults=' '.join(make_defaults(fcn_type.type_name_parts[:-1])),
                comma=',' if (i,j) != (0,0) else '', extra_field=extra_field_assign
            )
    print ']'

    print

    for fcn_type in fcn_types:
        print fn_lookup_name(fcn_type.fcn_type_alias_name), '= Dict.fromList ['
        for i, fcn in enumerate(fcn_type.members):
            print '    {1}"{0}" => {0}'.format(fcn.name, ',' if i > 0 else '')
        print '    ]'

    print

    print rec_name.lower() + 'Encoder thing = Encode.object'
    print '    [{extra_field} "name" => Encode.string thing.name, \n    "{kind}" => {kind}Encoder thing ]'.format(
        kind=kind.lower(), extra_field=extra_field_encode)

    print

    print kind.lower() + 'Encoder {{name, {kind}}} = Encode.object <|'.format(kind=kind.lower())
    print '    case', kind.lower(), 'of'
    for fcn_type in fcn_types:
        args = [ARG_LOOKUP[arg] for arg in fcn_type.type_name_parts[:-1]]
        arg_names = make_unique([arg['var_name'] for arg in args])
        print '        ', fcn_type.type_name, 'f', ' '.join(arg_names), '->'
        print '            [ "kind" => Encode.string "{}"'.format(fcn_type.type_name)
        print '            , "fn" => Encode.string name'
        for arg_name, encoder in zip(arg_names, [arg['encoder'] for arg in args]):
            print '            , "{arg_name}" => {encoder} {arg_name}'.format(
                arg_name=arg_name, encoder=encoder)
        print '            ]'

    print

    print """
    {- 
    The decoder pairs must be defined in this order and the lazy 
    decoding must go in the first one or we get runtime errors.
    
    Caused and fix as suggested by 
    https://github.com/elm-lang/elm-compiler/issues/1560
    -}
    """

    print kind.lower() + 'Decoder = Decode.oneOf'
    for i, fcn_type in enumerate(fcn_types):
        args = [ARG_LOOKUP[arg] for arg in fcn_type.type_name_parts[:-1]]
        arg_names = make_unique([arg['var_name'] for arg in args])
        print '        {token} Decode.map{num} {type}'.format(
            type=fcn_type.type_name, num=len(args) + 1 if len(args) else '', token='[' if i == 0 else ',')
        print '            (Decode.field "fn" <| fnDecoder {})'.format(fn_lookup_name(fcn_type.fcn_type_alias_name))
        for arg_name, decoder in zip(arg_names, [arg['decoder'] for arg in args]):
            print '            (Decode.field "{arg_name}" {decoder})'.format(
                arg_name=arg_name, decoder=decoder)
    print '        ]'

    print

    print rec_name.lower() + 'Decoder = Decode.map' + (str(3) if extra_rec_field else str(2)), rec_name
    print '    ', extra_field_decode
    print '    (Decode.field "name" Decode.string)'
    print '    (Decode.field "{kind}" {kind}Decoder)'.format(kind=kind.lower())

    print

