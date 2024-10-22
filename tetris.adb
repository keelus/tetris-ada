with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;

procedure Tetris is
    type Color_Type is (White, Yellow, Cyan, Red, Green, Purple, Transparent);

    package Random_Color_Type is new Ada.Numerics.Discrete_Random (Color_Type);
    use Random_Color_Type;
    Random_Color_Generator : Random_Color_Type.Generator;
    Random_Color           : Color_Type;

    -- Piece declaration
    type Piece_Kind is (J, L, S, T, Z, O); -- TODO: I

    package Random_Piece_Kind is new Ada.Numerics.Discrete_Random (Piece_Kind);
    use Random_Piece_Kind;
    Random_Kind_Generator : Random_Piece_Kind.Generator;
    Random_Kind           : Piece_Kind;

    -- Main piece types
    type Piece_Width is range 0 .. 2;
    type Piece_Height is range 0 .. 2;
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

    type Rows is range 0 .. Board_Height - 1;
    type Cols is range 0 .. Board_Width - 1;

    -- Board declaration
    type Board is array (Rows, Cols) of Color_Type;

    Current_Board : Board := (others => (others => Transparent));

    -- User input
    C : Character;

    function Get_Piece_Body (P : Piece) return Piece_Body is
        P_Body : Piece_Body;
    begin
        case P.Kind is
            when T =>
                case P.Rotation is
                    when 0 =>
                        P_Body := ((0, 1, 0), (1, 1, 1), (0, 0, 0));
                    when 1 =>
                        P_Body := ((0, 1, 0), (0, 1, 1), (0, 1, 0));
                    when 2 =>
                        P_Body := ((0, 0, 0), (1, 1, 1), (0, 1, 0));
                    when 3 =>
                        P_Body := ((0, 1, 0), (1, 1, 0), (0, 1, 0));
                end case;
            when L =>
                case P.Rotation is
                    when 0 =>
                        P_Body := ((0, 0, 1), (1, 1, 1), (0, 0, 0));
                    when 1 =>
                        P_Body := ((0, 1, 0), (0, 1, 0), (0, 1, 1));
                    when 2 =>
                        P_Body := ((0, 0, 0), (1, 1, 1), (1, 0, 0));
                    when 3 =>
                        P_Body := ((1, 1, 0), (0, 1, 0), (0, 1, 0));
                end case;
            when J =>
                case P.Rotation is
                    when 0 =>
                        P_Body := ((1, 0, 0), (1, 1, 1), (0, 0, 0));
                    when 1 =>
                        P_Body := ((0, 1, 1), (0, 1, 0), (0, 1, 0));
                    when 2 =>
                        P_Body := ((0, 0, 0), (1, 1, 1), (0, 0, 1));
                    when 3 =>
                        P_Body := ((0, 1, 0), (0, 1, 0), (1, 1, 0));
                end case;
            when S =>
                case P.Rotation is
                    when 0 =>
                        P_Body := ((0, 1, 1), (1, 1, 0), (0, 0, 0));
                    when 1 =>
                        P_Body := ((0, 1, 0), (0, 1, 1), (0, 0, 1));
                    when 2 =>
                        P_Body := ((0, 0, 0), (0, 1, 1), (1, 1, 0));
                    when 3 =>
                        P_Body := ((1, 0, 0), (1, 1, 0), (0, 1, 0));
                end case;
            when Z =>
                case P.Rotation is
                    when 0 =>
                        P_Body := ((1, 1, 0), (0, 1, 1), (0, 0, 0));
                    when 1 =>
                        P_Body := ((0, 0, 1), (0, 1, 1), (0, 1, 0));
                    when 2 =>
                        P_Body := ((0, 0, 0), (1, 1, 0), (0, 1, 1));
                    when 3 =>
                        P_Body := ((0, 1, 0), (1, 1, 0), (1, 0, 0));
                end case;
            when O =>
                P_Body := ((0, 1, 1), (0, 1, 1), (0, 0, 0));
            when others =>
                null;
        end case;
        return P_Body;
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
                        -- Good_Fall := False;
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

    procedure Place_Piece (P : Piece) is
        P_Body : Piece_Body;
        Cell_X : Integer;
        Cell_Y : Integer;
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
    end Place_Piece;

    function Create_Piece return Piece is
        P : Piece;
    begin
        P.X := 4;
        P.Y := 0;

        Reset (Random_Color_Generator);
        Reset (Random_Kind_Generator);

        P.Color := Random (Random_Color_Generator);
        P.Kind  := Random (Random_Kind_Generator);

        if P.Color = Transparent then
            P.Color := Green;
        end if;

        return P;
    end Create_Piece;

    Falling_Piece : Piece := Create_Piece;

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
        if P_Body (Piece_Height'Val (Y - P.Y), Piece_Width'Val (X - P.X)) = 1
        then
            return True;
        end if;
        return False;
    end Piece_Is_Cell;

    function Get_Piece_Width (P : Piece) return Piece_Width is
        P_Body : Piece_Body;
        Width  : Piece_Width := 1;
    begin
        P_Body := Get_Piece_Body (P);

        for I in Piece_Height loop
            for J in Piece_Width loop
                if P_Body (I, J) = 1 and J > Width then
                    Width := J;
                end if;
            end loop;
        end loop;
        return Width;
    end Get_Piece_Width;

    function Get_Piece_Height (P : Piece) return Piece_Height is
        P_Body : Piece_Body;
        Height : Piece_Height := 1;
    begin
        P_Body := Get_Piece_Body (P);

        for I in Piece_Height loop
            for J in Piece_Width loop
                if P_Body (I, J) = 1 and I > Height then
                    Height := I;
                end if;
            end loop;
        end loop;
        return Height;
    end Get_Piece_Height;

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
        Put (Ada.Characters.Latin_1.ESC & "[" &
            Ada.Strings.Fixed.Trim (Board_Height'Image, Ada.Strings.Left) &
            "A");
        Put (Ada.Characters.Latin_1.ESC & "[" &
            Ada.Strings.Fixed.Trim (Board_Width'Image, Ada.Strings.Left) &
            "D");
    end Clear_Screen;

    procedure Print_Screen is
    begin
        for I in Rows loop
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
                        Set_Ansi_Color (Cyan);
                        Put (".");
                    end if;
                end if;
                Put (" ");
            end loop;
            Set_Ansi_Color (White);
            Put_Line ("");
        end loop;
    end Print_Screen;

    task Print_Task;
    task body Print_Task is
    begin
        loop
            Print_Screen;
            delay 0.5;
            Clear_Screen;

            Falling_Piece.Y := Falling_Piece.Y + 1;

            if Is_Piece_Colliding (Falling_Piece) = Collision_Place then
                Falling_Piece.Y := Falling_Piece.Y - 1;

                Place_Piece (Falling_Piece);
                Falling_Piece := Create_Piece;
            end if;
        end loop;
    end Print_Task;
begin
    Main_Loop :
    loop
        Get_Immediate (Standard_Input, C);
        case C is
            when 'd' =>
                Falling_Piece.X := Falling_Piece.X + 1;

                if Is_Piece_Colliding (Falling_Piece) /= Collision_None then
                    Falling_Piece.X := Falling_Piece.X - 1;
                end if;
            when 'a' =>
                Falling_Piece.X := Falling_Piece.X - 1;

                if Is_Piece_Colliding (Falling_Piece) /= Collision_None then
                    Falling_Piece.X := Falling_Piece.X + 1;
                end if;
            when 'r' =>
                Falling_Piece.Rotation := Falling_Piece.Rotation + 1;

                if Is_Piece_Colliding (Falling_Piece) /= Collision_None then
                    Falling_Piece.Rotation := Falling_Piece.Rotation - 1;
                end if;
            when others =>
                null;
        end case;

        Clear_Screen;
        Print_Screen;
    end loop Main_Loop;
end Tetris;
