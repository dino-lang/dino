// The file describes extension of the abstract tree -- tree checked
// for context rules.

include "atree";

ext node {
  ext decl {
    var out_sym_p = 0;

    ext vdecl {
      var with_tp = 0, nested_use_p = 0; //???
    }
    ext par {
      var with_tp = 0, nested_use_p = 0; //???
    }
    ext field {
      var offset; // mpi ???
    }
  }
  
  ext tdef {
    var tp;
    var size, align; // size - mpi ???
    ext qualid { var decl; }
  }
  
  ext expr {
    var tp, lex;
    ext des {
      ext qualid {
	var decl;
      }
      ext field {
	var fdecl; // ???
      }
    }
  }
}
