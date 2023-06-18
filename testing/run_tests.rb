test_folder = "__tests__"
$compiler = "../bin/rel/zen"

COL_RED = "\e[31m"
COL_GREEN = "\e[32m"
RESET = "\e[0m"
TEXT_BOLD = "\e[1m"

puts "#{COL_RED}ZEN#{RESET} #{COL_GREEN}TESTER#{RESET}\n\n"

def test(folder)
  puts "Now testing in directory #{TEXT_BOLD}#{folder}...#{RESET}\n"
  Dir.foreach(folder) do |file|
    next if file == '.' or file == '..'
  
    file_path = File.join(folder, file)

    if File.directory?(file_path)
      puts "\n"
      test(file_path)
    elsif file_path.end_with?(".zn")
      print "Testing #{TEXT_BOLD}#{file_path}#{RESET}: "
      output = `#{$compiler} #{file_path}`

      expect = read_expected_output(file_path)

      if $?.success?
        if multiline_output_match?(output, expect)
          puts "#{COL_GREEN}PASSED#{RESET} with expected output"
        else
          puts "#{COL_GREEN}FAILED#{RESET} with unexpected output"
          puts "Expected:\n#{expect}"
          puts "Actual:\n#{output}"
        end
      else
        puts "#{COL_RED}FAILED#{RESET} with error"
        exit 1
      end
    end
  end
end

def read_expected_output(path)
  expected_output = ""
  read_output = false
  File.open(path, "r") do |file|
    file.each_line do |line|
      if line.start_with?("// expect:")
        read_output = true
        next
      elsif read_output && line.start_with?("// end expect")
        read_output = false
        break
      elsif read_output && line.start_with?("// ")
        expected_output += line.sub("// ", "").strip + "\n"
      end
    end
  end
  expected_output
end

def multiline_output_match?(actual_output, expected_output)
  actual_lines = actual_output.strip.split("\n")
  expected_lines = expected_output.strip.split("\n")

  actual_lines.size == expected_lines.size && actual_lines.zip(expected_lines).all? { |a, e| a.strip == e.strip }
end

test(test_folder)
puts "\n"
puts "All tests #{COL_GREEN}passed#{RESET}! :)"
