graph [
  version 2
  directed 1
  rankdir "LR"
  node [
    id 0
    name "1_pref_2_pref_3"
    label "(1) ≻ (2) ≻ (3)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(1) ≻ (2) ≻ (3)"
    ]
  ]
  node [
    id 1
    name "1_pref_2, 3"
    label "(1) ≻ (2, 3)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(1) ≻ (2, 3)"
    ]
  ]
  node [
    id 2
    name "1_pref_3_pref_2"
    label "(1) ≻ (3) ≻ (2)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(1) ≻ (3) ≻ (2)"
    ]
  ]
  node [
    id 3
    name "1, 2_pref_3"
    label "(1, 2) ≻ (3)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(1, 2) ≻ (3)"
    ]
  ]
  node [
    id 4
    name "1, 2, 3"
    label "(1, 2, 3)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(1, 2, 3)"
    ]
  ]
  node [
    id 5
    name "1, 3_pref_2"
    label "(1, 3) ≻ (2)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(1, 3) ≻ (2)"
    ]
  ]
  node [
    id 6
    name "2_pref_1_pref_3"
    label "(2) ≻ (1) ≻ (3)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(2) ≻ (1) ≻ (3)"
    ]
  ]
  node [
    id 7
    name "2_pref_3_pref_1"
    label "(2) ≻ (3) ≻ (1)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(2) ≻ (3) ≻ (1)"
    ]
  ]
  node [
    id 8
    name "2_pref_3, 1"
    label "(2) ≻ (3, 1)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(2) ≻ (3, 1)"
    ]
  ]
  node [
    id 9
    name "2, 3_pref_1"
    label "(2, 3) ≻ (1)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(2, 3) ≻ (1)"
    ]
  ]
  node [
    id 10
    name "3_pref_1_pref_2"
    label "(3) ≻ (1) ≻ (2)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(3) ≻ (1) ≻ (2)"
    ]
  ]
  node [
    id 11
    name "3_pref_2_pref_1"
    label "(3) ≻ (2) ≻ (1)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(3) ≻ (2) ≻ (1)"
    ]
  ]
  node [
    id 12
    name "3_pref_2, 1"
    label "(3) ≻ (2, 1)"
    graphics [
      type "box"
    ]
    LabelGraphics [
      text "(3) ≻ (2, 1)"
    ]
  ]
  edge [
    id 1
    source 0
    target 1
  ]
  edge [
    id 2
    source 0
    target 3
  ]
  edge [
    id 3
    source 1
    target 0
  ]
  edge [
    id 4
    source 1
    target 2
  ]
  edge [
    id 5
    source 1
    target 4
  ]
  edge [
    id 6
    source 2
    target 1
  ]
  edge [
    id 7
    source 2
    target 5
  ]
  edge [
    id 8
    source 3
    target 0
  ]
  edge [
    id 9
    source 3
    target 4
  ]
  edge [
    id 10
    source 3
    target 6
  ]
  edge [
    id 11
    source 4
    target 1
  ]
  edge [
    id 12
    source 4
    target 3
  ]
  edge [
    id 13
    source 4
    target 5
  ]
  edge [
    id 14
    source 4
    target 8
  ]
  edge [
    id 15
    source 4
    target 9
  ]
  edge [
    id 16
    source 4
    target 12
  ]
  edge [
    id 17
    source 5
    target 2
  ]
  edge [
    id 18
    source 5
    target 4
  ]
  edge [
    id 19
    source 5
    target 10
  ]
  edge [
    id 20
    source 6
    target 3
  ]
  edge [
    id 21
    source 6
    target 8
  ]
  edge [
    id 22
    source 7
    target 8
  ]
  edge [
    id 23
    source 7
    target 9
  ]
  edge [
    id 24
    source 8
    target 4
  ]
  edge [
    id 25
    source 8
    target 6
  ]
  edge [
    id 26
    source 8
    target 7
  ]
  edge [
    id 27
    source 9
    target 4
  ]
  edge [
    id 28
    source 9
    target 7
  ]
  edge [
    id 29
    source 9
    target 11
  ]
  edge [
    id 30
    source 10
    target 5
  ]
  edge [
    id 31
    source 10
    target 12
  ]
  edge [
    id 32
    source 11
    target 9
  ]
  edge [
    id 33
    source 11
    target 12
  ]
  edge [
    id 34
    source 12
    target 4
  ]
  edge [
    id 35
    source 12
    target 10
  ]
  edge [
    id 36
    source 12
    target 11
  ]
]
