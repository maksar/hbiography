require 'active_support/all'
require 'csv'

text = File.read("names.txt")
people = text.split("\n\n\n").map { |bio| bio.lines.map { |l| l.gsub(/\n/, '') } }

bios = people.map do |person|
  { "First Name" => person.shift, "Last Name" => person.shift }.merge(
    person.map do |cap|
      sp = cap.split(": ")
      [sp.shift, sp.join]
    end.to_h)
end

keys = ["First Name", "Last Name", "Nationality", "Date of Birth", "Place of Birth", "Parentage", "Family", "Education", "Qualifications", "Career", "Publications", "Date of Death", "Honours", "Leisure Interests", "Address", "Telephone", "Fax", "Achievements", "Email", "Films", "Dance", "Music", "Extended Family", "Plays", "Website", "Art Exhibitions", "Name at Birth", "Radio"]

CSV.open("_names.csv", "wb") do |csv|
  csv << keys
  bios.each do |hash|
    csv << hash.values_at(*keys)
  end
end

`cat _names.csv | uconv --add-signature -f UTF-8 -t UTF-8 > names.csv`
`rm _names.csv`

p bios.select { |b| b["First Name"].match(/[a-z]/) }