with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;         use Ada.Real_Time;
with GNAT.OS_Lib;

procedure Tetris is
    type Color_Type is (White, Yellow, Cyan, Red, Green, Purple, Transparent);

    package Random_Color_Type is new Ada.Numerics.Discrete_Random (Color_Type);
    use Random_Color_Type;
    Random_Color_Generator : Random_Color_Type.Generator;
    Random_Color           : Color_Type;

    -- Piece declaration
    type Piece_Kind is (J, L, S, T, Z, O, I);

    package Random_Piece_Kind is new Ada.Numerics.Discrete_Random (Piece_Kind);
    use Random_Piece_Kind;
    Random_Kind_Generator : Random_Piece_Kind.Generator;
    Random_Kind           : Piece_Kind;

    -- Main piece types
    type Piece_Width is range 0 .. 3;
    type Piece_Height is range 0 .. 3;
    type Piece_Cell is range 0 .. 1;
    type Piece_Body is array (Piece_Height, Piece_Width) of Piece_Cell;
    type Rotation_Type is mod 4;

    type Piece is record
        Kind     : Piece_Kind;
        Rotation : Rotation_Type;

        X : Integer;
        Y : Integer;

        Color : Color_Type;
    end record;

    -- Board size declaration
    Board_Width  : constant Positive := 10;
    Board_Height : constant Positive := 20;

    Visual_Board_Width  : constant Positive := 33;
    Visual_Board_Height : constant Positive := Board_Height + 2;

    type Rows is range 0 .. Board_Height - 1;
    type Cols is range 0 .. Board_Width - 1;

    -- Board declaration
    type Board is array (Rows, Cols) of Color_Type;

    Current_Board   : Board   := (others => (others => Transparent));
    Lines_Completed : Natural := 0;
    Game_Started    : Time;

    -- User input
    C : Character;

    type Piece_Bodies is array (Rotation_Type) of Piece_Body;

    T_Piece : Piece_Bodies :=
       (((0, 1, 0, 0), (1, 1, 1, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (0, 1, 1, 0), (0, 1, 0, 0), (0, 0, 0, 0)),
        ((0, 0, 0, 0), (1, 1, 1, 0), (0, 1, 0, 0), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (1, 1, 0, 0), (0, 1, 0, 0), (0, 0, 0, 0)));

    L_Piece : Piece_Bodies :=
       (((0, 0, 1, 0), (1, 1, 1, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 1, 0), (0, 0, 0, 0)),
        ((0, 0, 0, 0), (1, 1, 1, 0), (1, 0, 0, 0), (0, 0, 0, 0)),
        ((1, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0), (0, 0, 0, 0)));

    J_Piece : Piece_Bodies :=
       (((1, 0, 0, 0), (1, 1, 1, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
        ((0, 1, 1, 0), (0, 1, 0, 0), (0, 1, 0, 0), (0, 0, 0, 0)),
        ((0, 0, 0, 0), (1, 1, 1, 0), (0, 0, 1, 0), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (0, 1, 0, 0), (1, 1, 0, 0), (0, 0, 0, 0)));

    S_Piece : Piece_Bodies :=
       (((0, 1, 1, 0), (1, 1, 0, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (0, 1, 1, 0), (0, 0, 1, 0), (0, 0, 0, 0)),
        ((0, 0, 0, 0), (0, 1, 1, 0), (1, 1, 0, 0), (0, 0, 0, 0)),
        ((1, 0, 0, 0), (1, 1, 0, 0), (0, 1, 0, 0), (0, 0, 0, 0)));

    Z_Piece : Piece_Bodies :=
       (((1, 1, 0, 0), (0, 1, 1, 0), (0, 0, 0, 0), (0, 0, 0, 0)),
        ((0, 0, 1, 0), (0, 1, 1, 0), (0, 1, 0, 0), (0, 0, 0, 0)),
        ((0, 0, 0, 0), (1, 1, 0, 0), (0, 1, 1, 0), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (1, 1, 0, 0), (1, 0, 0, 0), (0, 0, 0, 0)));

    O_Piece : Piece_Body :=
       ((0, 1, 1, 0), (0, 1, 1, 0), others => (others => 0));

    I_Piece : Piece_Bodies :=
       (((0, 0, 0, 0), (1, 1, 1, 1), (0, 0, 0, 0), (0, 0, 0, 0)),
        ((0, 0, 1, 0), (0, 0, 1, 0), (0, 0, 1, 0), (0, 0, 1, 0)),
        ((0, 0, 0, 0), (0, 0, 0, 0), (1, 1, 1, 1), (0, 0, 0, 0)),
        ((0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0), (0, 1, 0, 0)));

    function Get_Piece_Body (P : Piece) return Piece_Body is
    begin
        case P.Kind is
            when T =>
                return T_Piece (P.Rotation);
            when L =>
                return L_Piece (P.Rotation);
            when J =>
                return J_Piece (P.Rotation);
            when S =>
                return S_Piece (P.Rotation);
            when Z =>
                return Z_Piece (P.Rotation);
            when O =>
                return O_Piece;
            when I =>
                return I_Piece (P.Rotation);
        end case;
    end Get_Piece_Body;

    type Collision_Type is (Collision_None, Collision_Edges, Collision_Place);

    function Is_Piece_Colliding (P : Piece) return Collision_Type is
        P_Body : Piece_Body;
        Cell_X : Integer;
        Cell_Y : Integer;
    begin
        P_Body := Get_Piece_Body (P);

        -- Check left and right borders of the board
        for I in Piece_Height loop
            for J in Piece_Width loop
                if P_Body (I, J) = 1 then
                    Cell_X := P.X + Natural'Val (J);

                    if Cell_X >= Natural'Val (Board_Width) then
                        return Collision_Edges;
                    end if;
                end if;
            end loop;
        end loop;

        -- Check bottom border of the board
        for I in Piece_Height loop
            for J in Piece_Width loop
                if P_Body (I, J) = 1 then
                    Cell_Y := Integer'Val (P.Y) + Integer'Val (I);

                    if Cell_Y >= Board_Height then
                        return Collision_Place;
                    end if;
                end if;
            end loop;
        end loop;

        -- Check collision with placed pieces
        for I in Piece_Height loop
            for J in Piece_Width loop
                if P_Body (I, J) = 1 then
                    Cell_X := Integer'Val (P.X) + Integer'Val (J);
                    Cell_Y := Integer'Val (P.Y) + Integer'Val (I);

                    if Cell_X < 0 then
                        return Collision_Place;
                    end if;
                    if Cell_X >= 0 and
                       Current_Board (Rows'Val (Cell_Y), Cols'Val (Cell_X)) /=
                          Transparent
                    then
                        return Collision_Place;
                    end if;
                end if;
            end loop;
        end loop;

        return Collision_None;
    end Is_Piece_Colliding;

    procedure Remove_Row (Target_I : Rows) is
        Reversed_I : Rows;
    begin
        Lines_Completed := Lines_Completed + 1;
        if Target_I = 0 then
            for J in Cols loop
                Current_Board (Target_I, J) := Transparent;
            end loop;
        else
            for I in Rows loop
                Reversed_I := Rows'Val (Board_Height - 1) - I;

                if Reversed_I > 0 and Reversed_I <= Target_I then
                    for J in Cols loop
                        Current_Board (Reversed_I, J) :=
                           Current_Board (Reversed_I - 1, J);
                        -- Current_Board (Reversed_I, J) := Green;
                    end loop;
                end if;
            end loop;
        end if;
    end Remove_Row;

    procedure Place_Piece (P : Piece) is
        P_Body : Piece_Body;
        Cell_X : Integer;
        Cell_Y : Integer;

        Entire_Row_Filled : Boolean;
    begin
        P_Body := Get_Piece_Body (P);

        for I in Piece_Height loop
            for J in Piece_Width loop
                if P_Body (I, J) = 1 then
                    Cell_X := Integer'Val (P.X) + Integer'Val (J);
                    Cell_Y := Integer'Val (P.Y) + Integer'Val (I);

                    Current_Board (Rows'Val (Cell_Y), Cols'Val (Cell_X)) :=
                       P.Color;
                end if;
            end loop;
        end loop;

        for I in Rows loop
            Entire_Row_Filled := True;

            for J in Cols loop
                if Current_Board (I, J) = Transparent then
                    Entire_Row_Filled := False;
                end if;
            end loop;

            if Entire_Row_Filled then
                Remove_Row (I);
            end if;
        end loop;
    end Place_Piece;

    function Create_Piece return Piece is
        P : Piece;
    begin
        P.X := 4;
        P.Y := 0;

        Reset (Random_Color_Generator);
        Reset (Random_Kind_Generator);

        P.Rotation := 0;

        P.Color := Random (Random_Color_Generator);
        P.Kind  := Random (Random_Kind_Generator);

        if P.Color = Transparent then
            P.Color := Green;
        end if;

        if Is_Piece_Colliding (P) /= Collision_None then
            Put_Line ("Game ended!");
            GNAT.OS_Lib.OS_Exit (0);
        end if;

        return P;
    end Create_Piece;

    Falling_Piece      : Piece := Create_Piece;
    Next_Falling_Piece : Piece := Create_Piece;

    function Piece_Is_Cell (P : Piece; X, Y : Natural) return Boolean is
        P_Body : Piece_Body;
    begin
        if X < P.X or Y < P.Y then
            return False;
        end if;
        if X > P.X + Natural'Val (Piece_Width'Last) or
           Y > P.Y + Natural'Val (Piece_Height'Last)
        then
            return False;
        end if;

        P_Body := Get_Piece_Body (P);
        return
           P_Body (Piece_Height'Val (Y - P.Y), Piece_Width'Val (X - P.X)) = 1;
    end Piece_Is_Cell;

    procedure Set_Ansi_Color (Color : Color_Type) is
        Ansi : Unbounded_String;
    begin
        Append (Ansi, Ada.Characters.Latin_1.ESC & "[");

        case Color is
            when Yellow =>
                Append (Ansi, "93m");
            when Cyan =>
                Append (Ansi, "96m");
            when Red =>
                Append (Ansi, "91m");
            when Green =>
                Append (Ansi, "92m");
            when Purple =>
                Append (Ansi, "95m");
            when others =>
                Append (Ansi, "97m");
        end case;

        Put (To_String (Ansi));
    end Set_Ansi_Color;

    -- Yoinked from https://github.com/tsoding/ada-gol
    procedure Clear_Screen is
    begin
        -- Put (Ada.Characters.Latin_1.ESC & "[2J");
        -- Put (Ada.Characters.Latin_1.ESC & "[H");
        Put (Ada.Characters.Latin_1.ESC & "[" &
            Ada.Strings.Fixed.Trim
               (Visual_Board_Height'Image, Ada.Strings.Left) &
            "A");
        Put (Ada.Characters.Latin_1.ESC & "[" &
            Ada.Strings.Fixed.Trim
               (Visual_Board_Width'Image, Ada.Strings.Left) &
            "D");
    end Clear_Screen;

    package IntIO is new Integer_IO (Natural);

    procedure Print_Screen is
        Time_Passed         : Duration;
        Time_Passed_Minutes : Integer := 0;
        Time_Passed_Seconds : Integer := 0;

        Next_Piece_Body : Piece_Body;
    begin
        if Clock /= Game_Started then
            Time_Passed := Duration (To_Duration (Clock - Game_Started));
            Time_Passed_Seconds :=
               Integer (Float'Floor (Float (Time_Passed))) mod 60;
            Time_Passed_Minutes :=
               Integer (Float'Floor (Float (Time_Passed) / 60.0));
            -- Time_Passed_Seconds := Integer'Val (Time_Passed);
        end if;

        Next_Piece_Body := Get_Piece_Body (Next_Falling_Piece);
        Put_Line ("          ╔═════════════════════╗");
        for I in Rows loop
            Set_Ansi_Color (White);
            Put ("          ║ ");
            for J in Cols loop
                if Piece_Is_Cell
                      (Falling_Piece, Natural'Val (J), Natural'Val (I))
                then
                    Set_Ansi_Color (Falling_Piece.Color);
                    Put ("■");
                else
                    if Current_Board (I, J) /= Transparent then
                        Set_Ansi_Color (Current_Board (I, J));
                        Put ("■");
                    else
                        Set_Ansi_Color (White);
                        Put (".");
                    end if;
                end if;
                Put (" ");
            end loop;
            Set_Ansi_Color (White);
            Put ("║");

            case I is
                when 1 | 6 | 14 =>
                    Put ("   ╔═══════════════╗");
                when 2 =>
                    Put ("   ║ Lines: ");
                    IntIO.Put (Item => Lines_Completed, Width => 6);
                    Put (" ║");
                when 3 =>
                    Put ("   ║ Time: ");
                    IntIO.Put (Item => Time_Passed_Minutes, Width => 2);
                    Put ("m ");
                    IntIO.Put (Item => Time_Passed_Seconds, Width => 2);
                    Put ("s ");
                    Put ("║");
                when 7 | 8 =>
                    if I = 7 then
                        Put ("   ║ Next:");
                    else
                        Put ("   ║      ");
                    end if;
                    Set_Ansi_Color (White);
                    for J in Piece_Width loop
                        if Next_Piece_Body (Piece_Height (I mod 7), J) = 1 then
                            Set_Ansi_Color (Next_Falling_Piece.Color);
                            Put (" ■");
                        else
                            Put ("  ");
                        end if;
                    end loop;
                    Set_Ansi_Color (White);
                    Put (" ║");
                when 15 =>
                    Put ("   ║ A, D: Move    ║");
                when 16 =>
                    Put ("   ║ R: Rotate     ║");
                when 17 =>
                    Put ("   ║ S: Fall       ║");
                when 4 | 9 | 18 =>
                    Put ("   ╚═══════════════╝");

                when others =>
                    null;
            end case;
            Put_Line ("");
        end loop;
        Put_Line ("          ╚═════════════════════╝");
        Set_Ansi_Color (White);
    end Print_Screen;

    protected Game_Controller is
        procedure Clear_And_Print_Screen;
        procedure Update_Board;
    private
        Has_Printed : Boolean := False;
    end Game_Controller;

    protected body Game_Controller is
        procedure Clear_And_Print_Screen is
        begin
            if Has_Printed then
                Clear_Screen;
            else
                Has_Printed := True;
            end if;

            Print_Screen;
        end Clear_And_Print_Screen;

        procedure Update_Board is
        begin
            Falling_Piece.Y := Falling_Piece.Y + 1;

            if Is_Piece_Colliding (Falling_Piece) = Collision_Place then
                Falling_Piece.Y := Falling_Piece.Y - 1;

                Place_Piece (Falling_Piece);
                Falling_Piece      := Next_Falling_Piece;
                Next_Falling_Piece := Create_Piece;
            end if;
        end Update_Board;
    end Game_Controller;

    task Print_Task;
    task body Print_Task is
    begin
        Game_Started := Clock;
        loop
            Game_Controller.Clear_And_Print_Screen;
            Game_Controller.Update_Board;
            delay 0.2;
        end loop;
    end Print_Task;

begin
    Main_Loop :
    loop
        Get_Immediate (Standard_Input, C);
        case C is
            when 'd' | 'D' =>
                Falling_Piece.X := Falling_Piece.X + 1;

                if Is_Piece_Colliding (Falling_Piece) /= Collision_None then
                    Falling_Piece.X := Falling_Piece.X - 1;
                end if;
            when 'a' | 'A' =>
                Falling_Piece.X := Falling_Piece.X - 1;

                if Is_Piece_Colliding (Falling_Piece) /= Collision_None then
                    Falling_Piece.X := Falling_Piece.X + 1;
                end if;
            when 'r' | 'R' =>
                Falling_Piece.Rotation := Falling_Piece.Rotation + 1;

                if Is_Piece_Colliding (Falling_Piece) /= Collision_None then
                    Falling_Piece.Rotation := Falling_Piece.Rotation - 1;
                end if;
            when 's' | 'S' =>
                Game_Controller.Update_Board;
            when others =>
                null;
        end case;

        Game_Controller.Clear_And_Print_Screen;
    end loop Main_Loop;
end Tetris;
