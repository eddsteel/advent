#!/bin/env ruby

class Doc
  attr :data

  def initialize(hash)
    @data = hash
  end

  def Doc::from_record(record)
    doc = {}
    record.split().map do |pair|
      parts = pair.split(':')
      doc[parts[0].to_sym]=parts[1]
    end
    Doc.new(doc)
  end

  def has_field(f)
    @data.include?(f)
  end

  def valid_passport?
    %i[byr iyr eyr hgt hcl ecl pid].all? do |f|
      @data.include?(f)
    end
  end

  def valid_range?(i, j, sym)
    begin
      (i..j).cover?(Integer(@data[sym]))
    rescue
      false
    end
  end

  def valid_byr?; valid_range?(1920, 2002, :byr); end
  def valid_iyr?; valid_range?(2010, 2020, :iyr); end
  def valid_eyr?; valid_range?(2020, 2030, :eyr); end

  def valid_hgt?
    case @data[:hgt]
    when /^([0-9]*)cm$/
      (150..193).cover?(Integer($1))
    when /^([0-9]*)in$/
      (59..76).cover?(Integer($1))
    else
      false
    end
  end

  def valid_hcl?
    /^#[0-9a-f]{6}$/.match?(@data[:hcl])
  end

  def valid_ecl?
    %w[amb blu brn gry grn hzl oth].include?(@data[:ecl])
  end

  def valid_pid?
    /^[0-9]{9}$/.match?(@data[:pid])
  end

  def passport_with_valid_fields?
    valid_passport? &&
      valid_byr? &&
      valid_iyr? &&
      valid_eyr? &&
      valid_hgt? &&
      valid_hcl? &&
      valid_ecl? &&
      valid_pid?
  end
end

def input_to_records(input)
  input.split("\n\n")
end

def count_passports(input)
  input_to_records(input).count do |record|
    doc = Doc.from_record(record)
    doc.valid_passport?
  end
end

def count_valid_passports(input)
  input_to_records(input).count do |record|
    doc = Doc.from_record(record)
    doc.passport_with_valid_fields?
  end
end

def run_examples_1
  examples=<<-EOF
  ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  byr:1937 iyr:2017 cid:147 hgt:183cm

  iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  hcl:#cfa07d byr:1929

  hcl:#ae17e1 iyr:2013
  eyr:2024
  ecl:brn pid:760753108 byr:1931
  hgt:179cm

  hcl:#cfa07d eyr:2025 pid:166559648
  iyr:2011 ecl:brn hgt:59in
  EOF

  puts "2 = #{count_passports(examples)}"
end

def run_examples_2a
  puts [Doc.new({byr: "2002"}).valid_byr?,
   !Doc.new({byr: "2003"}).valid_byr?,
   Doc.new({hgt: "60in"}).valid_hgt?,
   Doc.new({hgt: "190cm"}).valid_hgt?,
   !Doc.new({hgt: "190in"}).valid_hgt?,
   !Doc.new({hgt: "190"}).valid_hgt?,
   Doc.new({hcl: "#123abc"}).valid_hcl?,
   !Doc.new({hcl: "#123abz"}).valid_hcl?,
   !Doc.new({hcl: "123abc"}).valid_hcl?,
   Doc.new({ecl: "brn"}).valid_ecl?,
   !Doc.new({ecl: "wat"}).valid_ecl?,
   Doc.new({pid: "000000001"}).valid_pid?,
   !Doc.new({pid: "0123456789"}).valid_pid?]
end

def run_examples_2b
  false_examples=<<-EOF
  eyr:1972 cid:100
  hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

  iyr:2019
  hcl:#602927 eyr:1967 hgt:170cm
  ecl:grn pid:012533040 byr:1946

  hcl:dab227 iyr:2012
  ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

  hgt:59cm ecl:zzz
  eyr:2038 hcl:74454a iyr:2023
  pid:3556412378 byr:2007
  EOF
  true_examples=<<-EOF
  pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
  hcl:#623a2f

  eyr:2029 ecl:blu cid:129 byr:1989
  iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

  hcl:#888785
  hgt:164cm byr:2001 iyr:2015 cid:88
  pid:545766238 ecl:hzl
  eyr:2022

  iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
  EOF

  puts "0 = #{count_valid_passports(false_examples)}"
  puts "4 = #{count_valid_passports(true_examples)}"
end

def star_one
  count_passports(File.read('./input/4'))
end

def star_two
  count_valid_passports(File.read('./input/4'))
end

puts "answer 1: #{star_one}"
puts "answer 2: #{star_two}"
#run_examples_1
#run_examples_2a
#run_examples_2b
