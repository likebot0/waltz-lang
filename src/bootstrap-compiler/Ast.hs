module Ast where

import Global

data Node (a :: Symbol) (t :: Symbol) = Node
    { attributes :: Attributes a t
    , children :: Children a t
    }

type family Attributes (a :: Symbol) (t :: Symbol)

type family Children (a :: Symbol) (t :: Symbol)

type All a =
    [ Node a "block-expression"
    , Node a "discard"
    , Node a "expression"
    , Node a "grouped-expression"
    , Node a "identifier"
    , Node a "key-value"
    , Node a "lambda-expression"
    , Node a "root"
    , Node a "root/statement/base"
    , Node a "statement/let"
    , Node a "statement/include"
    , Node a "statement/if"
    , Node a "statement/with"
    , Node a "type-expression"
    ]
    :++: Literal a

type Literal a =
    [ Node a "literal/array"
    , Node a "literal/number"
    , Node a "literal/object"
    , Node a "literal/string"
    ]

type CurlyBracketsBody a =
    [Union
        [ Node a "discard"
        , Node a "statement/let"
        , Node a "statement/if"
        , Node a "statement/with"
        ]
    ]

type instance Children a "block-expression" =
    CurlyBracketsBody a

type instance Children a "discard" =
    Maybe (Node a "expression")

type instance Children a "expression" =
    [Union (
        [ Node a "block-expression"
        , Node a "grouped-expression"
        , Node a "identifier"
        , Node a "lambda-expression"
        , Node a "type-expression"
        ] :++: Literal a
    )]

type instance Children a "grouped-expression" =
    Node a "expression"

type instance Children a "identifier" =
    String

type instance Children a "key-value" =
    ( Node a "identifier"
    , Node a "expression"
    )

type instance Children a "lambda-expression" =
    ( Maybe (Node a "identifier")
    , CurlyBracketsBody a
    )

type instance Children a "literal/array" =
    [Union
        [ Node a "discard"
        , Node a "expression"
        , Node a "statement/let"
        , Node a "statement/if"
        , Node a "statement/include"
        , Node a "statement/with"
        ]
    ]

type instance Children a "literal/number" =
    String

type instance Children a "literal/object" =
    [Union
        [ Node a "discard"
        , Node a "key-value"
        , Node a "statement/let"
        , Node a "statement/if"
        , Node a "statement/include"
        , Node a "statement/with"
        ]
    ]

type instance Children a "literal/string" =
    [Union
        [ String
        , Node a "grouped-expression"
        ]
    ]

type instance Children a "root" =
    [Union
        [ Node a "expression"
        , Node a "root/statement/base"
        ]
    ]

type instance Children a "root/statement/base" =
    Node a "identifier"

type instance Children a "statement/if" =
    ( Node a "expression"
    , CurlyBracketsBody a
    )

type instance Children a "statement/let" =
    Node a "key-value"

type instance Children a "statement/with" =
    CurlyBracketsBody a

type instance Children a "type-expression" =
    Node a "expression"
