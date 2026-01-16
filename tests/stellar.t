Reinitialize:

  $ ./teardown.exe
  $ subsocia db-init --disable-core-schema

Core schema:

  $ subsocia at-create unique_name string
  $ subsocia au-force unique_name
  Created constraint #1.
  $ subsocia at-create proper_name string

Test schema:

  $ subsocia et-create organization
  $ subsocia et-create galaxy
  $ subsocia et-create star_system
  $ subsocia et-create star
  $ subsocia et-create planet
  $ subsocia et-create moon

  $ subsocia at-modify proper_name --display-cost 1000

  $ subsocia in-allow star		star_system
  $ subsocia in-allow star_system	star_system

  $ subsocia an-allow unique_name root organization
  $ subsocia an-allow unique_name organization galaxy
  $ subsocia an-allow unique_name galaxy star_system
  $ subsocia an-allow unique_name galaxy star
  $ subsocia an-allow unique_name star_system planet
  $ subsocia an-allow unique_name star planet

  $ subsocia an-allow proper_name root organization
  $ subsocia an-allow proper_name root galaxy
  $ subsocia an-allow proper_name root star_system
  $ subsocia an-allow proper_name root star
  $ subsocia an-allow proper_name root planet
  $ subsocia an-allow proper_name root moon

  $ subsocia at-create orbit int
  $ subsocia an-allow orbit star planet
  $ subsocia an-allow orbit planet moon

Inspect the schema:

  $ subsocia at-list
  unique_name : string*
  proper_name : string* (+1000)
  orbit : int*
  $ subsocia au-list
  {unique_name}
  $ subsocia an-list
  * orbit planet moon
  * orbit star planet
  * proper_name root moon
  * proper_name root planet
  * proper_name root star
  * proper_name root star_system
  * proper_name root galaxy
  * proper_name root organization
  * unique_name star planet
  * unique_name star_system planet
  * unique_name galaxy star
  * unique_name galaxy star_system
  * unique_name organization galaxy
  * unique_name root organization
  $ subsocia et-list
  root
  organization
  galaxy
  star_system
  star
  planet
  moon
  $ subsocia in-list
                     star_system ** star_system
                            star ** star_system

Create entities:

  $ subsocia create organization -a /s -a '/proper_name=Spatial Organization'
  $ subsocia create galaxy -a /s/milkyway -a '/proper_name=The Milky Way'
  $ subsocia create galaxy -a /s/adromeda -a '/proper_name=The Andromeda Galaxy'
  $ subsocia create galaxy -a /s/triangulum -a '/proper_name=The Triangulum Galaxy'
  $ subsocia create star -s /s/milkyway -a /s/milkyway/sun -a '/proper_name=The Sun'
  $ subsocia create star_system -s /s/milkyway -a /s/milkyway/alpha_centauri -a 'proper_name=Alpha Centauri'
  $ subsocia create star -s /s/milkyway/alpha_centauri -a /s/milkyway/rigil_kentaurus -a '/proper_name=Rigil Kentaurus'
  $ subsocia create star -s /s/milkyway/alpha_centauri -a /s/milkyway/toliman -a '/proper_name=Toliman'
  $ subsocia create star -s /s/milkyway/alpha_centauri -a /s/milkyway/proxima_centauri -a '/proper_name=Proxima Centauri'
  $ subsocia create planet -s /s/milkyway/proxima_centauri -a /s/milkyway/proxima_centauri/proxima_centauri_d -a '/proper_name=Proxima Centauri D' -a /s/milkyway/proxima_centauri/orbit=1
  $ subsocia create planet -s /s/milkyway/proxima_centauri -a /s/milkyway/proxima_centauri/proxima_centauri_b -a '/proper_name=Proxima Centauri B' -a /s/milkyway/proxima_centauri/orbit=2
  $ subsocia create planet -s /s/milkyway/proxima_centauri -a /s/milkyway/proxima_centauri/proxima_centauri_c -a '/proper_name=Proxima Centauri C' -a /s/milkyway/proxima_centauri/orbit=3

Inspect entities:

  $ subsocia ls /
  {unique_name=s}
  $ subsocia ls /s/milkyway
  {unique_name=sun}
  {unique_name=alpha_centauri}
  {unique_name=rigil_kentaurus}
  {unique_name=toliman}
  {unique_name=proxima_centauri}
  $ subsocia ls /s/milkyway/alpha_centauri
  $ subsocia ls /s/milkyway/proxima_centauri
  {unique_name=proxima_centauri_d}
  {unique_name=proxima_centauri_b}
  {unique_name=proxima_centauri_c}
  $ subsocia search /s/milkyway/+
  #6 The Sun : star
  #7 Alpha Centauri : star_system
  $ subsocia search /s/milkyway/+/+
  #8 Rigil Kentaurus : star
  #9 Toliman : star
  #10 Proxima Centauri : star
  $ subsocia search /s/milkyway/alpha_centauri/+
  #8 Rigil Kentaurus : star
  #9 Toliman : star
  #10 Proxima Centauri : star
  $ subsocia search /s/milkyway/proxima_centauri/+
  #11 Proxima Centauri D : planet
  #12 Proxima Centauri B : planet
  #13 Proxima Centauri C : planet
  $ subsocia search /s/milkyway/alpha_centauri/-
  #3 The Milky Way : galaxy
  $ subsocia search '/s/milkyway/proxima_centauri/orbit=_'
  #11 Proxima Centauri D : planet
  #12 Proxima Centauri B : planet
  #13 Proxima Centauri C : planet
  $ subsocia search '/s/milkyway/proxima_centauri/orbit<=2'
  #11 Proxima Centauri D : planet
  #12 Proxima Centauri B : planet
  $ subsocia search '/s/milkyway/proxima_centauri/orbit>=2'
  #12 Proxima Centauri B : planet
  #13 Proxima Centauri C : planet

Reinitialize:

  $ ./teardown.exe
  $ subsocia db-init
