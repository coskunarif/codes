# Set the local folder path where the PDF files are stored
$folderPath = "C:\temp\test"

# Initialize variables for storing the corrupted file names and count
$corruptedFileNames = @()
$corruptedFileCount = 0

# Get a list of all PDF files in the folder
$pdfFiles = Get-ChildItem -Path $folderPath -Filter "*.pdf"

# Loop through each PDF file
foreach ($pdfFile in $pdfFiles) {
  # Try to read the PDF file using the Adobe PDF library
  Try {
    $pdfDocument = New-Object -ComObject AcroExch.PDDoc
    $pdfDocument.Open($pdfFile.FullName)
  }
  Catch {
    # If the PDF file is corrupted, add its name to the list of corrupted files
    # and increment the count
    $corruptedFileNames += $pdfFile.Name
    $corruptedFileCount++
  }
}

# Output the names of the corrupted files and the total count
Write-Output "Corrupted files: $corruptedFileNames"
Write-Output "Total count: $corruptedFileCount"
