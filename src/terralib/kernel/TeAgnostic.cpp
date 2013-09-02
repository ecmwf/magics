#include "TeAgnostic.h"

namespace TeAgnostic{

  bool debugModeCheck()
  {
    #ifdef TEAGN_DEBUG_MODE
      return true;
    #else
      return false;
    #endif
  }

};
