with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure cal is
   -- Types for calendar representation
   type day_index is range 1..7;  -- Days of the week (1=Sun, 7=Sat)
   type week_index is range 1..6;  -- Up to 6 weeks in a month
   type month_number is range 1..12;
   type month_row_index is range 1..4;  -- 4 rows of months (3 months each)
   type month_col_index is range 1..3;  -- 3 columns of months
   
   -- Language type
   type language_type is (english, french);
   
   -- Calendar representation types
   type month_matrix is array (week_index, day_index) of Natural;
   type year_matrix is array (month_row_index, month_col_index) of month_matrix;
   
   -- Arrays for names
   type month_name_array is array (month_number) of String(1..9);
   type day_name_array is array (day_index) of String(1..2);
   
   -- Calendar data
   year_to_print : Integer;
   first_day_of_year : Integer;
   year_calendar : year_matrix;
   user_language : language_type := english;  -- Default to English
   
   -- Banner constants
   banner_height : constant := 10;
   banner_width : constant := 8;
   
   -- Month and day names
   month_names_en : constant month_name_array := 
     ("January  ", "February ", "March    ", "April    ",
      "May      ", "June     ", "July     ", "August   ",
      "September", "October  ", "November ", "December ");
      
   month_names_fr : constant month_name_array := 
     ("Janvier  ", "Fevrier  ", "Mars     ", "Avril    ",
      "Mai      ", "Juin     ", "Juillet  ", "Aout     ",
      "Septembre", "Octobre  ", "Novembre ", "Decembre ");
      
   day_names_en : constant day_name_array :=
     ("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa");
     
   day_names_fr : constant day_name_array :=
     ("Di", "Lu", "Ma", "Me", "Je", "Ve", "Sa");
   
   -- Function to determine if a year is valid in the Gregorian calendar range
   function is_valid(Year : in Integer) return Boolean is
   begin
      -- Gregorian calendar was adopted in 1582
      return Year >= 1582;
   end is_valid;
   
   -- Function to check if year is a leap year
   function leap_year(Year : in Integer) return Boolean is
   begin
      return (Year mod 4 = 0 and then Year mod 100 /= 0) or else Year mod 400 = 0;
   end leap_year;
   
   -- Function to get days in month
   function num_days_in_month(Month : in month_number; Year : in Integer) return Integer is
      Days : constant array (month_number) of Integer := 
        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
   begin
      if Month = 2 and then leap_year(Year) then
         return 29;
      else
         return Days(Month);
      end if;
   end num_days_in_month;
   
   -- Function to calculate first day of the year
   function calculate_first_day(Year : Integer) return Integer is
      Y : constant Integer := Year - 1;
      Result : Integer;
   begin
      -- Using the formula from requirements
      Result := (36 + Y + (Y / 4) - (Y / 100) + (Y / 400)) mod 7;
      
      -- Add 1 ONLY for non-leap years to correct the offset
      if not leap_year(Year) then
         Result := (Result ) mod 7;
      end if;
      
      return Result;
   end calculate_first_day;
   
   -- Procedure to read calendar info from user
   procedure read_cal_info(Year : out Integer; FirstDay : out Integer; Lang : out language_type) is
      Valid_Year : Boolean := False;
      Lang_Choice : Integer;
   begin
      while not Valid_Year loop
         Put("Enter year: ");
         Get(Year);
         
         if is_valid(Year) then
            Valid_Year := True;
         else
            Put_Line("Invalid year! The Gregorian calendar starts from 1582.");
         end if;
      end loop;
      
      -- Calculate the first day of the year using the formula
      FirstDay := calculate_first_day(Year);
    
      -- Ask for language preference
      Put_Line("Select language (1 for English, 2 for French): ");
      Get(Lang_Choice);
      if Lang_Choice = 2 then
         Lang := french;
      else
         Lang := english;
      end if;
   end read_cal_info;
   
   -- Procedure to display a large banner with the year
   procedure banner(Year : in Integer; Indent : in Integer) is
      Year_Str : constant String := Trim(Integer'Image(Year), Ada.Strings.Left);
      Spaces : constant String(1..Indent) := (others => ' ');
      
      -- Array to store digit patterns read from file
      type digit_pattern is array (0..9, 1..banner_height) of String(1..banner_width);
      digit_patterns : digit_pattern;
      
      -- File handling variables
      File : File_Type;
      Line : String(1..100);  -- Assuming no line is longer than 100 chars
      Last : Natural;
      Current_Digit : Integer := 0;
      Current_Line : Integer := 1;
   begin
      -- Read banner data from file
      begin
         Open(File, In_File, "banner.txt");
         
         while not End_Of_File(File) loop
            Get_Line(File, Line, Last);
            
            -- Check for digit marker lines (digit followed by colon)
            if Last >= 2 and then Line(1) >= '0' and then Line(1) <= '9' and then Line(2) = ':' then
               -- Start of a new digit definition
               Current_Digit := Character'Pos(Line(1)) - Character'Pos('0');
               Current_Line := 1;
            elsif Current_Line <= banner_height then
               -- Store the line for the current digit
               declare
                  Pattern_Line : String(1..banner_width) := (others => ' ');
               begin
                  -- Copy as much of the line as fits in banner_width
                  for I in 1..Integer'Min(Last, banner_width) loop
                     Pattern_Line(I) := Line(I);
                  end loop;
                  
                  -- Store the pattern
                  digit_patterns(Current_Digit, Current_Line) := Pattern_Line;
                  Current_Line := Current_Line + 1;
               end;
            end if;
         end loop;
         
         Close(File);
      exception
         when Name_Error =>
            Put_Line("Error: banner.txt not found!");
            return;
         when others =>
            Put_Line("Error reading banner data!");
            if Is_Open(File) then
               Close(File);
            end if;
            return;
      end;
      
      -- Print each line of the banner
      for Line_Num in 1..banner_height loop
         Put(Spaces);  -- Print indent
         
         -- Print each digit
         for I in Year_Str'Range loop
            -- Convert character to integer
            declare
               Digit : constant Integer := Character'Pos(Year_Str(I)) - Character'Pos('0');
            begin
               -- Print the banner line for this digit
               Put(digit_patterns(Digit, Line_Num));
            end;
         end loop;
         
         New_Line;
      end loop;
      New_Line;
   end banner;
   
   -- Procedure to fill a month matrix
   procedure fill_month(M : month_number; Y : Integer; 
                        Month_Cal : out month_matrix; First_Day : in out Integer) is
      Month_Days : Integer;
      Day_Count : Integer := 1;
      -- Adjust First_Day to be 1-based for our internal representation
      -- Sunday (0) becomes Day 1, Monday (1) becomes Day 2, etc.
      Internal_First_Day : constant Integer := First_Day + 1;
   begin
      -- Initialize to zeros
      for W in week_index loop
         for D in day_index loop
            Month_Cal(W, D) := 0;
         end loop;
      end loop;
      
      Month_Days := num_days_in_month(M, Y);
      
      -- Fill the calendar
      for W in week_index loop
         for D in day_index loop
            if (W = 1 and then Integer(D) < Internal_First_Day) or 
               (Day_Count > Month_Days) then
               Month_Cal(W, D) := 0;  -- Empty cell
            else
               Month_Cal(W, D) := Day_Count;
               Day_Count := Day_Count + 1;
            end if;
         end loop;
      end loop;
      
      -- Calculate the first day of the next month (0-6 representation)
      First_Day := (First_Day + num_days_in_month(M, Y)) mod 7;
   end fill_month;
   
   -- Procedure to build the entire calendar
   procedure build_calendar(Y : in Integer; First_Day : in Integer; 
                           Calendar : out year_matrix) is
      Current_First_Day : Integer := First_Day;
   begin
      -- Generate calendar data for each month of the year
      for M in month_number loop
         declare
            Row : constant month_row_index := month_row_index(((Integer(M) - 1) / 3) + 1);
            Col : constant month_col_index := month_col_index(((Integer(M) - 1) mod 3) + 1);
         begin
            fill_month(M, Y, Calendar(Row, Col), Current_First_Day);
         end;
      end loop;
   end build_calendar;
   
   -- Procedure to print row heading for a row of months
   procedure print_row_heading(Row : in month_row_index; Y : in Integer; Lang : in language_type) is
      pragma Unreferenced (Y);  -- Mark Y as intentionally unreferenced
      Month_Names : month_name_array;
      Day_Names : day_name_array;
   begin
      -- Select appropriate language
      if Lang = french then
         Month_Names := month_names_fr;
         Day_Names := day_names_fr;
      else
         Month_Names := month_names_en;
         Day_Names := day_names_en;
      end if;
      
      -- Print month names
      for Col in month_col_index loop
         declare
            M : constant Integer := (Integer(Row) - 1) * 3 + Integer(Col);
         begin
            if M <= 12 then
               Put("       " & Month_Names(month_number(M)));
               Put("       "); -- Add spacing between months
            end if;
         end;
      end loop;
      New_Line;
      
      -- Print day headers
      for Col in month_col_index loop
         for D in day_index loop
            Put(Day_Names(D));
            Put(' ');
         end loop;
         Put("  "); -- Small gap between months
      end loop;
      New_Line;
   end print_row_heading;
   
   -- Procedure to print a row of months
   procedure print_row_month(Row : in month_row_index; Calendar : in year_matrix) is
   begin
      for W in week_index loop
         for Col in month_col_index loop
            for D in day_index loop
               declare
                  Value : constant Natural := Calendar(Row, Col)(W, D);
               begin
                  if Value = 0 then
                     -- Print 3 spaces for blank days
                     Put("   ");
                  else
                     -- Print day in a 2-char wide field, then a space
                     if Value < 10 then
                        Put(" ");
                     end if;
                     Put(Value, Width => 1);
                     Put(' ');
                  end if;
               end;
            end loop;
            -- Small gap between months
            Put("  ");
         end loop;
         New_Line;
      end loop;
   end print_row_month;
   
   -- Procedure to print the entire year in a 4x3 calendar format
   procedure print_year(Y : Integer; Calendar : year_matrix; Lang : language_type) is
   begin
      -- Display the banner for the year
      banner(Y, 20);  -- Indent by 20 spaces for centering
      
      if Lang = french then
         Put_Line("Annee " & Integer'Image(Y));
      else
         Put_Line("Year " & Integer'Image(Y));
      end if;
      New_Line;

      -- Print each row of months (4 rows, each with 3 months)
      for Row in month_row_index loop
         print_row_heading(Row, Y, Lang);
         print_row_month(Row, Calendar);
         New_Line;
      end loop;
   end print_year;

begin
   -- Read calendar info from user
   read_cal_info(year_to_print, first_day_of_year, user_language);
   
   -- Build the calendar
   build_calendar(year_to_print, first_day_of_year, year_calendar);
   
   -- Print the full year calendar
   print_year(year_to_print, year_calendar, user_language);
end cal;