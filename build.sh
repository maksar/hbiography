stack build && cat names.txt | ./.stack-work/dist/x86_64-osx/Cabal-1.24.0.0/build/hbiography-exe/hbiography-exe +RTS -N8 -RTS -s

echo '"First Name","Last Name","Nationality","Date of Birth","Name at Birth","Place of Birth","Date of Death","Address","Telephone","Fax","Email","Website","Family","Parentage","Extended Family","Education","Qualifications","Career","Honours","Achievements","Publications","Leisure Interests","Radio","TV","Films","Music","Dance","Plays","Art Exhibitions"' > names.csv
cat names.csv_ >> names.csv

rm names.csv_