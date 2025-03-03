with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Cal is
   -- Types for calendar representation
   type Day_Index is range 1..7;  -- Days of the week (1=Sun, 7=Sat)
   type Week_Index is range 1..6;  -- Up to 6 weeks in a month
   type Month_Number is range 1..12;
   type Month_Row_Index is range 1..4;  -- 4 rows of months (3 months each)
   type Month_Col_Index is range 1..3;  -- 3 columns of months
   
   -- Language type
   type Language_Type is (English, French);
   
   -- Calendar representation types
   type Month_Matrix is array (Week_Index, Day_Index) of Natural;
   type Year_Matrix is array (Month_Row_Index, Month_Col_Index) of Month_Matrix;
   
   -- Arrays for names
   type Month_Name_Array is array (Month_Number) of String(1..9);
   type Day_Name_Array is array (Day_Index) of String(1..2);
   
   -- Calendar data
   Year_To_Print : Integer;
   First_Day_Of_Year : Integer;
   Year_Calendar : Year_Matrix;
   User_Language : Language_Type := English;  -- Default to English
   
   -- Month and day names
   Month_Names_EN : constant Month_Name_Array := 
     ("January  ", "February ", "March    ", "April    ",
      "May      ", "June     ", "July     ", "August   ",
      "September", "October  ", "November ", "December ");
      
   Month_Names_FR : constant Month_Name_Array := 
     ("Janvier  ", "Fevrier  ", "Mars     ", "Avril    ",
      "Mai      ", "Juin     ", "Juillet  ", "Aout     ",
      "Septembre", "Octobre  ", "Novembre ", "Decembre ");
      
   Day_Names_EN : constant Day_Name_Array :=
     ("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa");
     
   Day_Names_FR : constant Day_Name_Array :=
     ("Di", "Lu", "Ma", "Me", "Je", "Ve", "Sa");
   
   -- Function to determine if a year is valid in the Gregorian calendar range
   function IsValid(Year : in Integer) return Boolean is
   begin
      -- Gregorian calendar was adopted in 1582
      return Year >= 1582;
   end IsValid;
   
   -- Function to check if year is a leap year
   function LeapYear(Year : in Integer) return Boolean is
   begin
      return (Year mod 4 = 0 and then Year mod 100 /= 0) or else Year mod 400 = 0;
   end LeapYear;
   
   -- Function to get days in month
   function NumDaysInMonth(Month : in Month_Number; Year : in Integer) return Integer is
      Days : constant array (Month_Number) of Integer := 
        (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
   begin
      if Month = 2 and then LeapYear(Year) then
         return 29;
      else
         return Days(Month);
      end if;
   end NumDaysInMonth;
   
   -- Function to calculate first day of the year
   function Calculate_First_Day(Year : Integer) return Integer is
      Y : constant Integer := Year - 1;
      Result : Integer;
   begin
      -- Using the formula from requirements
      Result := (36 + Y + (Y / 4) - (Y / 100) + (Y / 400)) mod 7;
      
      -- Add 1 ONLY for non-leap years to correct the offset
      if not LeapYear(Year) then
         Result := (Result ) mod 7;
      end if;
      
      return Result;
   end Calculate_First_Day;
   
   -- Procedure to read calendar info from user
   procedure ReadCalInfo(Year : out Integer; FirstDay : out Integer; Lang : out Language_Type) is
      Valid_Year : Boolean := False;
      Lang_Choice : Integer;
   begin
      while not Valid_Year loop
         Put("Enter year: ");
         Get(Year);
         
         if IsValid(Year) then
            Valid_Year := True;
         else
            Put_Line("Invalid year! The Gregorian calendar starts from 1582.");
         end if;
      end loop;
      
      -- Calculate the first day of the year using the formula
      FirstDay := Calculate_First_Day(Year);
    
      
      -- Ask for language preference
      Put_Line("Select language (1 for English, 2 for French): ");
      Get(Lang_Choice);
      if Lang_Choice = 2 then
         Lang := French;
      else
         Lang := English;
      end if;
   end ReadCalInfo;
   
   -- Procedure to fill a month matrix
   procedure Fill_Month(M : Month_Number; Y : Integer; 
                        Month_Cal : out Month_Matrix; First_Day : in out Integer) is
      Month_Days : Integer;
      Day_Count : Integer := 1;
      -- Adjust First_Day to be 1-based for our internal representation
      -- Sunday (0) becomes Day 1, Monday (1) becomes Day 2, etc.
      Internal_First_Day : constant Integer := First_Day + 1;
   begin
      -- Initialize to zeros
      for W in Week_Index loop
         for D in Day_Index loop
            Month_Cal(W, D) := 0;
         end loop;
      end loop;
      
      Month_Days := NumDaysInMonth(M, Y);
      
      -- Fill the calendar
      for W in Week_Index loop
         for D in Day_Index loop
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
      First_Day := (First_Day + NumDaysInMonth(M, Y)) mod 7;
   end Fill_Month;
   
   -- Procedure to build the entire calendar
   procedure BuildCalendar(Y : in Integer; First_Day : in Integer; 
                           Calendar : out Year_Matrix) is
      Current_First_Day : Integer := First_Day;
   begin
      -- Generate calendar data for each month of the year
      for M in Month_Number loop
         declare
            Row : constant Month_Row_Index := Month_Row_Index(((Integer(M) - 1) / 3) + 1);
            Col : constant Month_Col_Index := Month_Col_Index(((Integer(M) - 1) mod 3) + 1);
         begin
            Fill_Month(M, Y, Calendar(Row, Col), Current_First_Day);
         end;
      end loop;
   end BuildCalendar;
   
   -- Procedure to print row heading for a row of months
   procedure PrintRowHeading(Row : in Month_Row_Index; Y : in Integer; Lang : in Language_Type) is
      pragma Unreferenced (Y);  -- Mark Y as intentionally unreferenced
      Month_Names : Month_Name_Array;
      Day_Names : Day_Name_Array;
   begin
      -- Select appropriate language
      if Lang = French then
         Month_Names := Month_Names_FR;
         Day_Names := Day_Names_FR;
      else
         Month_Names := Month_Names_EN;
         Day_Names := Day_Names_EN;
      end if;
      
      -- Print month names
      for Col in Month_Col_Index loop
         declare
            M : constant Integer := (Integer(Row) - 1) * 3 + Integer(Col);
         begin
            if M <= 12 then
               Put("       " & Month_Names(Month_Number(M)));
               Put("       "); -- Add spacing between months
            end if;
         end;
      end loop;
      New_Line;
      
      -- Print day headers
      for Col in Month_Col_Index loop
         for D in Day_Index loop
            Put(Day_Names(D));
            Put(' ');
         end loop;
         Put("  "); -- Small gap between months
      end loop;
      New_Line;
   end PrintRowHeading;
   
   -- Procedure to print a row of months
   procedure PrintRowMonth(Row : in Month_Row_Index; Calendar : in Year_Matrix) is
   begin
      for W in Week_Index loop
         for Col in Month_Col_Index loop
            for D in Day_Index loop
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
   end PrintRowMonth;
   
   -- Procedure to print the entire year in a 4x3 calendar format
   procedure Print_Year(Y : Integer; Calendar : Year_Matrix; Lang : Language_Type) is
   begin
      if Lang = French then
         Put_Line("Annee " & Integer'Image(Y));
      else
         Put_Line("Year " & Integer'Image(Y));
      end if;
      New_Line;

      -- Print each row of months (4 rows, each with 3 months)
      for Row in Month_Row_Index loop
         PrintRowHeading(Row, Y, Lang);
         PrintRowMonth(Row, Calendar);
         New_Line;
      end loop;
   end Print_Year;

begin
   -- Read calendar info from user
   ReadCalInfo(Year_To_Print, First_Day_Of_Year, User_Language);
   
   -- Build the calendar
   BuildCalendar(Year_To_Print, First_Day_Of_Year, Year_Calendar);
   
   -- Print the full year calendar
   Print_Year(Year_To_Print, Year_Calendar, User_Language);
end Cal;