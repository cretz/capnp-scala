# addressbook.capnp
@0x9eb32e19f86ee174;
$import "/capnp/c++.capnp".namespace("addressbook");
struct Person @0x98808e9832e8bc18 {  # 8 bytes, 4 ptrs
  id @0 :UInt32;  # bits[0, 32)
  name @1 :Text;  # ptr[0]
  email @2 :Text;  # ptr[1]
  phones @3 :List(PhoneNumber);  # ptr[2]
  employment :group {
    union {  # tag bits [32, 48)
      unemployed @4 :Void;  # bits[0, 0), union tag = 0
      employer @5 :Text;  # ptr[3], union tag = 1
      school @6 :Text;  # ptr[3], union tag = 2
      selfEmployed @7 :Void;  # bits[0, 0), union tag = 3
    }
  }
  struct PhoneNumber @0x814e90b29c9e8ad0 {  # 8 bytes, 1 ptrs
    number @0 :Text;  # ptr[0]
    type @1 :Type;  # bits[0, 16)
    enum Type @0x91e0bd04d585062f {
      mobile @0;
      home @1;
      work @2;
    }
  }
}
struct AddressBook @0xf934d9b354a8a134 {  # 0 bytes, 1 ptrs, packed as pointer
  people @0 :List(Person);  # ptr[0]
}
