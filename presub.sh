#!/bin/bash

# Dolphin Compiler Pre-submission Check Script
# Tests student submissions for basic consistency
# Usage: ./presub.sh <phase_number> <zip_file>

set -euo pipefail  # Exit on error, undefined variables, and pipe failures

# Configuration
readonly SCRIPT_NAME=$(basename "$0")
readonly FILES_REQUIRED_ALL=("ast.ml" "deserializer.ml" "deserializer.mli" "compiler.mli" "main.ml")
readonly FILES_REQUIRED_PHASE3=("location.ml" "location.mli")

# Exit codes
readonly EXIT_SUCCESS=0
readonly EXIT_CRITICAL=1
readonly EXIT_USAGE=2

# Initialize status tracking
declare -A CHECK_STATUS=(
    [files]=0
    [compile_project]=0
    [compile_dlp]=0
)
declare -a ISSUES_FOUND=()

# Color codes with terminal support detection
setup_colors() {
    if [ -t 1 ] && [ "${TERM:-dumb}" != "dumb" ]; then
        readonly RED='\033[0;31m'
        readonly GREEN='\033[0;32m'
        readonly YELLOW='\033[0;33m'
        readonly BLUE='\033[0;34m'
        readonly NC='\033[0m'  # No Color
    else
        readonly RED=''
        readonly GREEN=''
        readonly YELLOW=''
        readonly BLUE=''
        readonly NC=''
    fi
}

# Usage function
usage() {
    cat << EOF
Usage: $SCRIPT_NAME <phase_number> <zip_file>

Tests a Dolphin compiler submission for basic consistency.

Arguments:
    phase_number  Phase number (1-5)
    zip_file      Path to the submission zip file

Options:
    -h, --help    Show this help message

Example:
    $SCRIPT_NAME 3 my_project.zip

Exit codes:
    0  All checks passed
    1  Critical failure (high likelihood of deviation from the assignment specification)
    2  Usage error

EOF
    exit $EXIT_USAGE
}

# Print colored message with consistent format
print_msg() {
    local color="$1"
    local status="$2"
    local message="$3"
    printf "%b[%s]%b %s\n" "$color" "$status" "$NC" "$message"
}

# Print step indicator
print_step() {
    local step="$1"
    local total="$2"
    local description="$3"
    printf "\n%b[Step %d/%d]%b %s\n" "$BLUE" "$step" "$total" "$NC" "$description"
}

# Setup cleanup trap
setup_cleanup_trap() {
    local items="$*"
    trap "rm -rf $items" EXIT INT TERM
}

# Get test program for phase
get_test_program() {
    if [ "$PHASE" -le 3 ]; then
        echo "return 0;"
    else
        echo "int main () { return 0; }"
    fi
}

# Get test program description for phase
get_test_program_desc() {
    if [ "$PHASE" -le 3 ]; then
        echo "'return 0;'"
    else
        echo "'int main() { return 0; }'"
    fi
}

# Get required files for phase
get_required_files() {
    local -a files=("${FILES_REQUIRED_ALL[@]}")
    if [ "$PHASE" -ge 3 ]; then
        files+=("${FILES_REQUIRED_PHASE3[@]}")
    fi
    echo "${files[@]}"
}

# Validate command line arguments
validate_args() {
    # Check for help flag
    if [[ "${1:-}" == "-h" ]] || [[ "${1:-}" == "--help" ]]; then
        usage
    fi
    
    # Check correct number of arguments
    if [ $# -ne 2 ]; then
        print_msg "$RED" "Error" "Incorrect number of arguments"
        echo "Expected 2 arguments, got $#"
        echo "Run '$SCRIPT_NAME --help' for usage information"
        exit $EXIT_USAGE
    fi
    
    PHASE="$1"
    INPUT_ZIP="$2"
    
    # Validate phase number
    if ! [[ "$PHASE" =~ ^[1-5]$ ]]; then
        print_msg "$RED" "Error" "Invalid phase number: $PHASE"
        echo "Phase must be a number between 1 and 5"
        exit $EXIT_USAGE
    fi
    
    # Validate zip file exists
    if [ ! -f "$INPUT_ZIP" ]; then
        print_msg "$RED" "Error" "Zip file not found: $INPUT_ZIP"
        exit $EXIT_USAGE
    fi
}

# Check if a file exists and is not empty
check_file() {
    local filename="$1"
    local required_from_phase="${2:-1}"
    
    # Skip if not required for this phase
    if [ "$PHASE" -lt "$required_from_phase" ]; then
        return 0
    fi
    
    # Find the file (excluding _build directory)
    local filepath
    filepath=$(find "$tmpdir" -type d -name "_build" -prune -o -name "$filename" -type f -print 2>/dev/null | head -1) || true
    
    if [ -z "$filepath" ]; then
        print_msg "$RED" "Missing" "Required file $filename not found"
        CHECK_STATUS[files]=1
        ISSUES_FOUND+=("Missing file: $filename")
        return 1
    elif [ ! -s "$filepath" ]; then
        print_msg "$YELLOW" "Warning" "File $filename exists but is empty"
        CHECK_STATUS[files]=1
        ISSUES_FOUND+=("Empty file: $filename")
        return 1
    else
        print_msg "$GREEN" "Ok" "File $filename is present"
        return 0
    fi
}

# Extract and setup project
setup_project() {
    # Create temporary directory
    tmpdir=$(mktemp -d "/tmp/dolphin_submission_XXXXXX") || {
        print_msg "$RED" "Error" "Failed to create temporary directory"
        exit $EXIT_CRITICAL
    }
    
    # Setup cleanup trap
    setup_cleanup_trap "$tmpdir"
    
    # Extract zip file
    if ! unzip -q "$INPUT_ZIP" -d "$tmpdir" 2>/dev/null; then
        print_msg "$RED" "Error" "Failed to extract zip file"
        echo "Please ensure the file is a valid zip archive"
        exit $EXIT_CRITICAL
    fi
    
    # Find project root (directory containing dune-project)
    local dune_project
    dune_project=$(find "$tmpdir" -name 'dune-project' -type f 2>/dev/null | head -1) || true
    
    if [ -z "$dune_project" ]; then
        print_msg "$RED" "Error" "No dune-project file found in submission"
        echo "Please ensure your submission includes a dune-project file"
        exit $EXIT_CRITICAL
    fi
    
    project_root=$(dirname "$dune_project")
    cd "$project_root" || {
        print_msg "$RED" "Error" "Failed to change to project directory"
        exit $EXIT_CRITICAL
    }
}

# Check all required files
check_required_files() {
    local check_failed=0
    local required_files
    read -ra required_files <<< "$(get_required_files)"
    
    for file in "${required_files[@]}"; do
        # Determine required phase for file
        local required_phase=1
        for phase3_file in "${FILES_REQUIRED_PHASE3[@]}"; do
            if [[ "$file" == "$phase3_file" ]]; then
                required_phase=3
                break
            fi
        done
        
        check_file "$file" "$required_phase" || check_failed=1
    done
    
    return $check_failed
}

# Compile the project
compile_project() {
    # Setup opam environment if available
    if command -v opam &> /dev/null; then
        eval "$(opam env)" 2>/dev/null || true
    fi
    
    # Try to compile
    if dune build 2>/dev/null; then
        print_msg "$GREEN" "Ok" "Project compiled successfully"
        return 0
    else
        print_msg "$RED" "Fail" "Project compilation failed"
        CHECK_STATUS[compile_project]=1
        ISSUES_FOUND+=("Project compilation failed")
        echo "Run 'dune build' locally to see detailed error messages"
        return 1
    fi
}

# Test minimal program compilation
test_minimal_program() {
    local tmpdlp
    tmpdlp=$(mktemp "/tmp/test_XXXXXX.dlp") || {
        print_msg "$YELLOW" "Warning" "Could not create test file"
        CHECK_STATUS[compile_dlp]=1
        ISSUES_FOUND+=("Could not test minimal program")
        return 1
    }
    
    # Update cleanup trap
    setup_cleanup_trap "${tmpdir:-}" "${tmpdlp:-}"
    
    # Create appropriate test program for phase
    local test_program
    test_program=$(get_test_program)
    echo "$test_program" > "$tmpdlp"
    
    local program_desc
    program_desc=$(get_test_program_desc)
    
    # Test compilation
    if dune exec dolphin -- rescue --phase "$PHASE" "$tmpdlp" --output /dev/null 2>/dev/null; then
        print_msg "$GREEN" "Ok" "Minimal program ($program_desc) compiled successfully"
        rm -f "$tmpdlp"
        return 0
    else
        print_msg "$YELLOW" "Warning" "Failed to compile minimal program ($program_desc)"
        CHECK_STATUS[compile_dlp]=1
        ISSUES_FOUND+=("Minimal program compilation failed")
        rm -f "$tmpdlp"
        return 1
    fi
}

# Get exit code based on check results
get_exit_code() {
    # Critical failure: project won't compile
    if [ "${CHECK_STATUS[compile_project]}" -eq 1 ]; then
        return $EXIT_CRITICAL
    fi
    
    # Missing required files
    if [ "${CHECK_STATUS[files]}" -eq 1 ]; then
        return $EXIT_CRITICAL
    fi
    
    # All checks passed or only minor issues
    return $EXIT_SUCCESS
}

# Print final summary
print_summary() {
    printf "\n%s\n" "========================================"
    print_msg "$BLUE" "Summary" "Submission check complete for Phase $PHASE"
    printf "%s\n" "----------------------------------------"
    
    # Calculate statistics
    local total_checks=${#CHECK_STATUS[@]}
    local failed_checks=0
    for status in "${CHECK_STATUS[@]}"; do
        ((failed_checks += status))
    done
    local passed_checks=$((total_checks - failed_checks))
    
    printf "Checks passed: %d/%d\n" "$passed_checks" "$total_checks"
    
    # Print issues if any
    if [ ${#ISSUES_FOUND[@]} -gt 0 ]; then
        printf "\n%bIssues found:%b\n" "$YELLOW" "$NC"
        for issue in "${ISSUES_FOUND[@]}"; do
            printf "  • %s\n" "$issue"
        done
    fi
    
    # Determine final status
    printf "\n%bResult:%b " "$BLUE" "$NC"
    
    get_exit_code
    local exit_code=$?
    
    if [ ${#ISSUES_FOUND[@]} -eq 0 ]; then
        print_msg "$GREEN" "Success" "All checks passed!"
        printf "%bYou can submit this zip.%b\n" "$GREEN" "$NC"
    elif [ $exit_code -eq $EXIT_CRITICAL ]; then
        print_msg "$RED" "Cannot Submit" "Critical issues found"
        echo "Please fix the issues before submitting."
    else
        print_msg "$YELLOW" "Minor Issues" "Non-critical problems detected"
        echo "You can submit this zip at your own risk."
    fi
    
    return $exit_code
}

# Main execution function
main() {
    # Setup
    setup_colors
    validate_args "$@"
    
    # Print header
    printf "\n%b=== Dolphin Submission Test ===%b\n" "$BLUE" "$NC"
    printf "Testing: %s\n" "$INPUT_ZIP"
    printf "Phase:   %s\n" "$PHASE"
    
    # Step 1: Extract and setup
    print_step 1 5 "Extracting submission archive..."
    setup_project
    
    # Step 2: Check required files
    print_step 2 5 "Checking required files..."
    check_required_files || true
    
    # Step 3: Compile project
    print_step 3 5 "Compiling project with dune..."
    compile_project || true
    
    # Step 4: Test minimal program (only if project compiled)
    if [ "${CHECK_STATUS[compile_project]}" -eq 0 ]; then
        print_step 4 5 "Testing minimal program compilation..."
        test_minimal_program || true
    else
        print_step 4 5 "Skipping minimal program test (project compilation failed)..."
        CHECK_STATUS[compile_dlp]=1
        ISSUES_FOUND+=("Minimal program test skipped")
    fi
    
    # Step 5: Summary
    print_step 5 5 "Generating report..."
    print_summary
    exit_code=$?
    
    printf "%s\n" "========================================"
    
    exit $exit_code
}

# Run main function with all arguments
main "$@"