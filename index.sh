#! /bin/sh

# saved into ~/.index unless used inline
# index configure name "INDEX" # also -n or --name
# index configure editor "nano" # also -e or --editor
# index configure align left left # also -a or --align, smashed together when used with a single argument
# index configure prefix continued "  " # also -r or --prefix
# index configure prefix listed "  "
# index configure prefix added "+ "
# index configure prefix removed "- "
# index configure prefix updated "! "
# index configure prefix error "? "
# index configure infix " - " # also -i or --infix
# index configure postfix "" # also -o or --postfix

# index edit

# index list # like find, but returns all on no arguments

# index find music configurations pictures/ ./documents nothing\
#   music          - sequential notes
#   documents      - books, papers and other text documents
# ? pictures       - not indexed
#   configurations - reusable system and software
#                    configurations
# ? nothing        - not present

# index remove ./nothing/ music
# ? nothing - not indexed
# - music   - sequential notes

# index add .\projects\ "all kinds" of "waste"
# + projects - all kinds of waste

# index update ./music\ "representations of pressure waves"
# ! music - representations of pressure waves
