# preflight() switches to tidyarena's packages by default when not interactive

    Code
      out <- preflight("duckplyr", "session", FALSE, .libPaths())
    Message
      Checking that "duckplyr" loads.
      ! "duckplyr" does not load with your packages:
        fake needs dplyr >= 9.0, but your library has 1.2.1.
      i Using tidyarena's newer packages for this run, for every backend.

