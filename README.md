# EXIFupdate
<b>Update EXIF data of JPG/JPEG pictures from Yuneec Typhoon H (CGO3+)</b>
![Screenshot](/doc/Screenshot_Tab.png)
Collected data

 * Path to FlightLog,
 * Flight number in FlightLog,
 * Complete data lines from Telemetry, Remote and RemoteGPS for,
   - Take-off,
   - Took picture.
 * Exported Model without the icon file (analog, curves, manifest, switches),
 * Gimbal pitch, roll and yaw from XMP data,
 * Distance or drone position from take-off to first picture and then from picture to picture [m],
 * Own text added with additional information.

Data correlation done by timestamp. Additional simple crosscheck by GPS position, to be near. How near, this can be tuned by a slider “Variance” between 10cm and 200cm.
The related object in JSON data will only appear if data available.

Input:
- Folder with JPG pictures,
- FlightLog, original file structure, folder name must contain “FlightLog”,
- Exported model from ST16 (same level or deeper as FlightLog in file system),
- XMP data package from original EXIF meta data,
- own entries in the text fields of the application.
Output:	
Updated EXIF data in JPG pictures from CGO3+.

All data will be written in EXIF “UserComment” as JSON to JPG/JPEG picture file. You can keep the original “UserComment” or overwrite it (default).


Developed with Lazarus IDE 2.0.10   https://www.lazarus-ide.org/

Compiled with FPC 3.2.0

Needed component:    https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/fpexif/


EXIF tags:    https://exiftool.org/TagNames/EXIF.html

JSON schema:  http://json-schema.org/
