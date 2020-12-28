{Bulgarian GUI}

resourcestring

  capForm='Обновяване на EXIF данните в JPG файлове, получени с помощта на CGO3+';
  capScan='Търсене';
  hntScan='Търсене на снимки и добавяне на EXIF, получени от FlightLog';
  capClose='Затваряне';
  capBackup='Съхраняване на архивни файлове (бекап)';
  hntBackup='Запазване на оригиналните снимки като архивни.';
  capVariance='Девиация';
  hntDelta='Максимално отклонение от позицията в сантиметри между даннит от EXIF и координатите от FlightLog';
  capLogs='Телеметрия';                                {Open a telemetry file}
  hntLogs='Изберете файла с темелемтрични данни от съответстващия FlightLog';
  capPics='Снимки';                                    {Select folder for pictures}
  hntPics='Изберете папка с JPG файлове получени с помощта на CGO3+';
  capOpenDialog='Изберете файл с теметрични данни...';
  capTextDialog='Изберете текст файл...';
  capDirDialog='Изберете папка със снимки... ';
  capSaveTab='Save table as...';
  capController='Роля на контролера';
  capSaveCSV='Запазете таблицата като CSV';
  capLog='Създайте log файл';
  hntLog='Създайте регистрационен файл в CSV формат в папката на проекта';
  capGeoid='Geoid undulation';
  hntGeoid='Correction value from Ellipsoid to Geoid';
  capSettings='Settings';
  capMain='Protocol';
  capEXIF='EXIF data handling';
  capFiles='File handling';
  capUpdateAlt='Update altitude';
  hntUpdateAlt='Correct altitude to absolute altitude instead of reltive altitude';
  capJSON='Create JSON';
  hntJSON='Create additional JSON files per picure file';
  capEXIFwrite='Update EXIF';
  hntEXIFwrite='Enable writing EXIF data into the picture file';

  capKeepComment='Запазете оригиналния UserComment';
  hntKeepComment='Запазете оригиналния потребителски коментар в EXIF като част от UserComment';
  hntMemo='Текст с допълнителна информация в EXIF като част от UserComment';
  capSetInfo='AdditionalText позволен';
  hntSetInfo='Add free text to UserComment in EXIF meta data';
  capLoad='Зареждане от текстов файл';
  capPaste='Поставете (Ctrl+V)';
  capClear='Изтриване на текст';

  rsHeader='Име на файл,дата/час,EXIF версия,Координати,Variance,Altitude,Telemetry съотвествие,'+
           'Remote,RemoteGPS,модел,XMP,Допълнителен текст,Update';
  rsLogHeader='File name,File date,EXIF date,Type,Sequence No,Variance [cm],'+
              'Latitude,Longitude,Altitude [m],Ascent [m],Distance [m],Bearing [°]';
  rsNo='Не';
  rsYes='done';

  rsExtFilter='CSV файл (по умълчание)|*.csv|текст файл|*.txt|всички файлове|*.*';
  rsNoFiles='Не са намерени файлове със снимки';
  rsNoFlight='Не е намерен моментът на излитане';
  rsMissLog='Липсва FlightLog';
  rsSavedTo='запазено като';

  rgitem0='Single mode';
  rgitem1='Team mode main';
  rgitem2='Team mode camera';
  rgitem3='Wizard';
  rgitem4='Самоопределен';
