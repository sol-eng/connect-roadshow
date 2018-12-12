#ifndef dplyr_tools_SymbolString_h
#define dplyr_tools_SymbolString_h

#include <tools/Encoding.h>

namespace dplyr {

  class SymbolString  {
  public:
    SymbolString() {}

    SymbolString(const String& other) : s(other) {}

    SymbolString(const String::StringProxy& other) : s(other) {}

    SymbolString(const String::const_StringProxy& other) : s(other) {}

    // Symbols are always encoded in the native encoding (#1950)
    explicit SymbolString(const Symbol& symbol) : s(CHAR(PRINTNAME(symbol)), CE_NATIVE) {}

  public:
    const String& get_string() const {
      return s;
    }

    const Symbol get_symbol() const {
      return Symbol(Rf_translateChar(s.get_sexp()));
    }

    const std::string get_cstring() const {
      return s.get_cstring();
    }

    SEXP get_sexp() const {
      return s.get_sexp();
    }

  private:
    String s;
  };

}

#endif
