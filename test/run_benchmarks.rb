require "open3"
require "./os"

$bench_folder = "benchmark"
$compiler = if OS.windows?
              "../bin/rel/zen.exe"
            else
              "../bin/rel/zen"
            end

$benchmarks = 0

COL_RED = "\e[31m"
COL_GREEN = "\e[32m"
RESET = "\e[0m"
TEXT_BOLD = "\e[1m"

puts "#{COL_RED}ZEN#{RESET} #{COL_GREEN}BENCHMARKER#{RESET}\n\n"

# Run benchmarks in a folder.
def bench(folder)
  puts "Running benchmarks in directory #{TEXT_BOLD}#{folder}...#{RESET}\n"

  Dir.foreach(folder) do |file|
    next if file == '.' or file == '..'

    file_path = File.join(folder, file)

    if File.directory?(file_path)
      puts "\n"
      bench(file_path)
    elsif file_path.end_with?(".zn")
      is_draft = check_if_draft(file_path)

      if is_draft
        next
      end

      print "Benchmarking #{TEXT_BOLD}#{file_path}#{RESET}: "

      time, error, status = capture_output("#{$compiler} #{file_path}")

      if status != 0
        puts "#{COL_RED}ERROR#{RESET} while benchmarking:"
        puts error
      else
        puts "Got time: #{COL_GREEN}#{time}#{RESET}"
        $benchmarks += 1
      end
    end
  end
end

def check_if_draft(path)
  is_draft = false

  File.open(path, "r") do |file|
    file.each_line do |line|
      if line.strip == "// DRAFT"
        is_draft = true
        break
      end
    end
  end

  return is_draft
end

def capture_output(command)
  output = ""
  error = ""
  exit_status = 0

  Open3.popen3(command) do |stdin, stdout, stderr, wait_thr|
    output = stdout.read
    error = stderr.read
    exit_status = wait_thr.value
  end

  # The time is always printed at the end in the benchmark programs.
  time = output.split("\n").last

  [time, error, exit_status]
end

bench($bench_folder)
puts ""

puts "Succesfully benchmarked #{$benchmarks} files."
