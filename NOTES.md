Allocations can be reduced with a C stub, but the existing OCaml code will do for now.

We will likely add an `attach` operation to map a cstruct onto a buffer, which is a good point to do a single bounds check and let all subsequent accesses be direct.  This will require generating an abstract type for the cstruct, and probably inline the existing Cstruct.BE and LE modules.

Add support in cenum for specifying the current position of the value (e.g. field=5;) to skip unused blocks (as suggested in #1).
