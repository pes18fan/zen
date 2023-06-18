require "open3"

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
      output, error, status = capture_output("#{$compiler} #{file_path}")

      expect, expected_err, wants_err = read_expected_output(file_path)

      if status == 0
        if multiline_output_match?(output, expect)
          puts "#{COL_GREEN}PASSED#{RESET} with expected output"
        else
          puts "#{COL_GREEN}FAILED#{RESET} with unexpected output"
          puts "Expected:\n#{expect}"
          puts "Actual:\n#{output}"
        end
      else
        if wants_err
          if error.strip.include?(expected_err)
            puts "#{COL_GREEN}PASSED#{RESET} with expected error"
          else
            puts "#{COL_RED}FAILED#{RESET} with unexpected error"
            puts "Expected:\n#{expected_err}"
            puts "Got:\n#{error}"
            exit 1
          end
        end
      end
    end
  end
end

def read_expected_output(path)
  expected_output = ""
  expected_error = ""
  read_output = false
  read_error = false
  wants_error = false

  File.open(path, "r") do |file|
    file.each_line do |line|
      if line.strip == "// expect:"
        read_output = true
        next
      elsif read_output && line.strip == "// end expect"
        read_output = false
        break
      elsif line.strip == "// expect error:"
        read_error = true
        wants_error = true
        next
      elsif read_error && line.strip == "// end expect error"
        read_error = false
        next
      elsif read_error && line.start_with?("// ")
        expected_error += line.sub("// ", "")
      elsif read_output && line.start_with?("// ")
        expected_output += line.sub("// ", "")
      end
    end
  end

  [expected_output.strip, expected_error.strip, wants_error]
end

def multiline_output_match?(actual_output, expected_output)
  actual_lines = actual_output.strip.split("\n")
  expected_lines = expected_output.strip.split("\n")

  actual_lines.size == expected_lines.size && actual_lines.zip(expected_lines).all? { |a, e| a.strip == e.strip }
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
puts "All tests #{COL_GREEN}passed#{RESET}! :)"
