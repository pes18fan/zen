require "open3"

test_folder = "__tests__"
$compiler = "../bin/rel/zen"

$tests = 0
$passed = 0
$failed = 0

COL_RED = "\e[31m"
COL_GREEN = "\e[32m"
RESET = "\e[0m"
TEXT_BOLD = "\e[1m"

puts "#{COL_RED}ZEN#{RESET} #{COL_GREEN}TESTER#{RESET}\n\n"

# Test files recursively.
def test(folder)
  puts "Now testing in directory #{TEXT_BOLD}#{folder}...#{RESET}\n"
  Dir.foreach(folder) do |file|
    next if file == '.' or file == '..'
  
    file_path = File.join(folder, file)

    if File.directory?(file_path)
      puts "\n"
      test(file_path)
    elsif file_path.end_with?(".zn")
      expect, expected_err, wants_err, is_draft = 
        read_expected_output(file_path)

      # Skip if the current file is a draft
      if is_draft
        next
      end

      print "Testing #{TEXT_BOLD}#{file_path}#{RESET}: "

      output, error, status = capture_output("#{$compiler} #{file_path}")

      if status == 0
        if multiline_output_match?(output, expect)
          puts "#{COL_GREEN}PASSED#{RESET} with expected output"
          $passed += 1
        else
          puts "#{COL_GREEN}FAILED#{RESET} with unexpected output"
          puts "Expected:\n#{expect}"
          puts "Actual:\n#{output}"
          $failed += 1
        end
      else
        if wants_err
          if error.strip.include?(expected_err)
            puts "#{COL_GREEN}PASSED#{RESET} with expected error"
            $passed += 1
          else
            puts "#{COL_RED}FAILED#{RESET} with unexpected error"
            puts "Expected:\n#{expected_err}"
            puts "Got:\n#{error}"
            $failed += 1
          end
        end
      end

      $tests += 1
    end
  end
end

def read_expected_output(path)
  expected_output = ""
  expected_error = ""
  read_output = false
  wants_error = false
  is_draft = false

  File.open(path, "r") do |file|
    file.each_line do |line|
      if line.strip == "// DRAFT"
        is_draft = true
        break
      elsif line.strip == "// expect:"
        read_output = true
        next
      elsif read_output && line.strip == "// end expect"
        read_output = false
        break
      elsif read_output && line.start_with?("// ERR: ")
        wants_error = true
        expected_error += line.sub("// ERR: ", "")
      elsif read_output && line.start_with?("// ")
        expected_output += line.sub("// ", "")
      end
    end
  end

  [expected_output.strip, expected_error.strip, wants_error, is_draft]
end

def multiline_output_match?(actual_output, expected_output)
  actual_lines = actual_output.strip.split("\n")
  expected_lines = expected_output.strip.split("\n")

  actual_lines.size == expected_lines.size &&
    actual_lines.zip(expected_lines).all? { |a, e| a.strip == e.strip }
end

# Capture stderr and stdout seperately
def capture_output(command)
  output = ""
  error = ""
  exit_status = 0

  Open3.popen3(command) do |stdin, stdout, stderr, wait_thr|
    output = stdout.read
    error = stderr.read
    exit_status = wait_thr.value
  end

  [output, error, exit_status]
end

test(test_folder)
puts "\n"

puts "Total tests: #{$tests}"
if $failed > 0
  puts "#{COL_RED}FAILED#{RESET}: #{$failed} tests failed."
elsif $passed == $tests
  puts "All tests #{COL_GREEN}PASSED!#{RESET} :)"
else
  puts "#{COL_RED}Something went wrong.#{RESET}"
  puts "#{$passed} tests passed, #{$failed} failed."
end
