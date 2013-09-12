# ../capnproto-c++-0.3.0/src/capnp/schema.capnp
@0xa93fc509624c72d9;
$import "/../capnproto-c++-0.3.0/src/capnp/c++.capnp".namespace("capnp::schema");
struct Node @0xe682ab4cf923a417 {  # 40 bytes, 5 ptrs
  id @0 :UInt64;  # bits[0, 64)
  displayName @1 :Text;  # ptr[0]
  displayNamePrefixLength @2 :UInt32;  # bits[64, 96)
  scopeId @3 :UInt64;  # bits[128, 192)
  nestedNodes @4 :List(NestedNode);  # ptr[1]
  annotations @5 :List(Annotation);  # ptr[2]
  union {  # tag bits [96, 112)
    file @6 :Void;  # bits[0, 0), union tag = 0
    struct :group {, union tag = 1
      dataWordCount @7 :UInt16;  # bits[112, 128)
      pointerCount @8 :UInt16;  # bits[192, 208)
      preferredListEncoding @9 :ElementSize;  # bits[208, 224)
      isGroup @10 :Bool;  # bits[224, 225)
      discriminantCount @11 :UInt16;  # bits[240, 256)
      discriminantOffset @12 :UInt32;  # bits[256, 288)
      fields @13 :List(Field);  # ptr[3]
    }
    enum :group {, union tag = 2
      enumerants @14 :List(Enumerant);  # ptr[3]
    }
    interface :group {, union tag = 3
      methods @15 :List(Method);  # ptr[3]
    }
    const :group {, union tag = 4
      type @16 :Type;  # ptr[3]
      value @17 :Value;  # ptr[4]
    }
    annotation :group {, union tag = 5
      type @18 :Type;  # ptr[3]
      targetsFile @19 :Bool;  # bits[112, 113)
      targetsConst @20 :Bool;  # bits[113, 114)
      targetsEnum @21 :Bool;  # bits[114, 115)
      targetsEnumerant @22 :Bool;  # bits[115, 116)
      targetsStruct @23 :Bool;  # bits[116, 117)
      targetsField @24 :Bool;  # bits[117, 118)
      targetsUnion @25 :Bool;  # bits[118, 119)
      targetsGroup @26 :Bool;  # bits[119, 120)
      targetsInterface @27 :Bool;  # bits[120, 121)
      targetsMethod @28 :Bool;  # bits[121, 122)
      targetsParam @29 :Bool;  # bits[122, 123)
      targetsAnnotation @30 :Bool;  # bits[123, 124)
    }
  }
  struct NestedNode @0xdebf55bbfa0fc242 {  # 8 bytes, 1 ptrs
    name @0 :Text;  # ptr[0]
    id @1 :UInt64;  # bits[0, 64)
  }
}
struct Field @0x9aad50a41f4af45f {  # 24 bytes, 4 ptrs
  name @0 :Text;  # ptr[0]
  codeOrder @1 :UInt16;  # bits[0, 16)
  annotations @2 :List(Annotation);  # ptr[1]
  discriminantValue @3 :UInt16 = 65535;  # bits[16, 32)
  union {  # tag bits [64, 80)
    slot :group {, union tag = 0
      offset @4 :UInt32;  # bits[32, 64)
      type @5 :Type;  # ptr[2]
      defaultValue @6 :Value;  # ptr[3]
    }
    group :group {, union tag = 1
      typeId @7 :UInt64;  # bits[128, 192)
    }
  }
  ordinal :group {
    union {  # tag bits [80, 96)
      implicit @8 :Void;  # bits[0, 0), union tag = 0
      explicit @9 :UInt16;  # bits[96, 112), union tag = 1
    }
  }
}
struct Enumerant @0x978a7cebdc549a4d {  # 8 bytes, 2 ptrs
  name @0 :Text;  # ptr[0]
  codeOrder @1 :UInt16;  # bits[0, 16)
  annotations @2 :List(Annotation);  # ptr[1]
}
struct Method @0x9500cce23b334d80 {  # 8 bytes, 4 ptrs
  name @0 :Text;  # ptr[0]
  codeOrder @1 :UInt16;  # bits[0, 16)
  params @2 :List(Param);  # ptr[1]
  requiredParamCount @3 :UInt16;  # bits[16, 32)
  returnType @4 :Type;  # ptr[2]
  annotations @5 :List(Annotation);  # ptr[3]
  struct Param @0xcbc0c86dae91fcf6 {  # 0 bytes, 4 ptrs
    name @0 :Text;  # ptr[0]
    type @1 :Type;  # ptr[1]
    defaultValue @2 :Value;  # ptr[2]
    annotations @3 :List(Annotation);  # ptr[3]
  }
}
struct Type @0xd07378ede1f9cc60 {  # 16 bytes, 1 ptrs
  union {  # tag bits [0, 16)
    void @0 :Void;  # bits[0, 0), union tag = 0
    bool @1 :Void;  # bits[0, 0), union tag = 1
    int8 @2 :Void;  # bits[0, 0), union tag = 2
    int16 @3 :Void;  # bits[0, 0), union tag = 3
    int32 @4 :Void;  # bits[0, 0), union tag = 4
    int64 @5 :Void;  # bits[0, 0), union tag = 5
    uint8 @6 :Void;  # bits[0, 0), union tag = 6
    uint16 @7 :Void;  # bits[0, 0), union tag = 7
    uint32 @8 :Void;  # bits[0, 0), union tag = 8
    uint64 @9 :Void;  # bits[0, 0), union tag = 9
    float32 @10 :Void;  # bits[0, 0), union tag = 10
    float64 @11 :Void;  # bits[0, 0), union tag = 11
    text @12 :Void;  # bits[0, 0), union tag = 12
    data @13 :Void;  # bits[0, 0), union tag = 13
    list :group {, union tag = 14
      elementType @14 :Type;  # ptr[0]
    }
    enum :group {, union tag = 15
      typeId @15 :UInt64;  # bits[64, 128)
    }
    struct :group {, union tag = 16
      typeId @16 :UInt64;  # bits[64, 128)
    }
    interface :group {, union tag = 17
      typeId @17 :UInt64;  # bits[64, 128)
    }
    object @18 :Void;  # bits[0, 0), union tag = 18
  }
}
struct Value @0xce23dcd2d7b00c9b {  # 16 bytes, 1 ptrs
  union {  # tag bits [0, 16)
    void @0 :Void;  # bits[0, 0), union tag = 0
    bool @1 :Bool;  # bits[16, 17), union tag = 1
    int8 @2 :Int8;  # bits[16, 24), union tag = 2
    int16 @3 :Int16;  # bits[16, 32), union tag = 3
    int32 @4 :Int32;  # bits[32, 64), union tag = 4
    int64 @5 :Int64;  # bits[64, 128), union tag = 5
    uint8 @6 :UInt8;  # bits[16, 24), union tag = 6
    uint16 @7 :UInt16;  # bits[16, 32), union tag = 7
    uint32 @8 :UInt32;  # bits[32, 64), union tag = 8
    uint64 @9 :UInt64;  # bits[64, 128), union tag = 9
    float32 @10 :Float32;  # bits[32, 64), union tag = 10
    float64 @11 :Float64;  # bits[64, 128), union tag = 11
    text @12 :Text;  # ptr[0], union tag = 12
    data @13 :Data;  # ptr[0], union tag = 13
    list @14 :Object;  # ptr[0], union tag = 14
    enum @15 :UInt16;  # bits[16, 32), union tag = 15
    struct @16 :Object;  # ptr[0], union tag = 16
    interface @17 :Void;  # bits[0, 0), union tag = 17
    object @18 :Object;  # ptr[0], union tag = 18
  }
}
struct Annotation @0xf1c8950dab257542 {  # 8 bytes, 1 ptrs
  id @0 :UInt64;  # bits[0, 64)
  value @1 :Value;  # ptr[0]
}
enum ElementSize @0xd1958f7dba521926 {
  empty @0;
  bit @1;
  byte @2;
  twoBytes @3;
  fourBytes @4;
  eightBytes @5;
  pointer @6;
  inlineComposite @7;
}
struct CodeGeneratorRequest @0xbfc546f6210ad7ce {  # 0 bytes, 2 ptrs
  nodes @0 :List(Node);  # ptr[0]
  requestedFiles @1 :List(RequestedFile);  # ptr[1]
  struct RequestedFile @0xcfea0eb02e810062 {  # 8 bytes, 2 ptrs
    id @0 :UInt64;  # bits[0, 64)
    filename @1 :Text;  # ptr[0]
    imports @2 :List(Import);  # ptr[1]
    struct Import @0xae504193122357e5 {  # 8 bytes, 1 ptrs
      id @0 :UInt64;  # bits[0, 64)
      name @1 :Text;  # ptr[0]
    }
  }
}