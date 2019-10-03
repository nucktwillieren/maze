implicit none

    integer i,j,k,io,route_count,ways_count,intersection_count
    integer x_boundary,y_boundary,start_x,start_y,end_x,end_y,posi_x,posi_y
    character(len=300) filename,linein
    character(len=1) single
    integer, dimension (40000) :: intersection_x,intersection_y
    integer, dimension (:,:), allocatable :: maze_darray,route_darray  !動態陣列

    filename = "maze_20by20"  !改檔名就可以換迷宮了
    x_boundary = 0
    y_boundary = 0

    !!!!先找出迷宮長跟寬!!!!

    open(1,file=filename,form="formatted")
    do
        read(1,"(a)",IOSTAT=io) linein
        if (io /= 0) exit
        y_boundary = y_boundary + 1
        if (len_trim(linein) > x_boundary) then 
            x_boundary = len_trim(linein)
        endif
    enddo

    close(1)

    !!!!!!!!!!!!!!!!!!!!
    
    !!!!分配陣列大小!!!

    allocate(maze_darray(x_boundary,y_boundary))
    allocate(route_darray(x_boundary,y_boundary))

    !!!!!!!!!!!!!!!!!
    
    !!!!找牆壁跟空格!!!!!

    open(1,file=filename,form="formatted")
    do i=1,y_boundary
        read(1,"(a)") linein
        do j=1,x_boundary
            single = linein(j:j)
            if (single == ' ') then
                maze_darray(j,i) = 0
            else
                maze_darray(j,i) = 1
            endif
        enddo
    enddo

    close(1)

    !!!!!!!!!!!!!!!!!!

    !!!!!自動找迷宮的出口跟入口!!!!!!!!!

    do i = 1,y_boundary
        if (maze_darray(1,i)==0) then
            start_x = 1
            start_y = i
            maze_darray(1,i) = 4
        else if (maze_darray(x_boundary,i)==0) then
            end_x = x_boundary
            end_y = i
        endif
    enddo

    do i = 1,x_boundary
        if (maze_darray(i,1)==0) then
            start_x = i
            start_y = 1
            maze_darray(i,1) = 4
        else if (maze_darray(i,y_boundary)==0) then
            end_x = i
            end_y = y_boundary
        endif
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!!!!印出原本迷宮的長相!!!!!!!!!!

    do i = 1,y_boundary
        do j = 1,x_boundary
            write(*,'(i1)',advance='no') maze_darray(j,i)
        enddo
        print*,
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    
    !!!!!!!!!!讓可宜上起點!!!!!!!!!!

    posi_x = start_x
    posi_y = start_y
    route_count = 0
    intersection_count = 1

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



    !!!!!!!!!開始走迷宮!!!!!!!!!!!!
    do 

        !!!!!! 判斷這個地方是不是 十字路口 !!!!!!
        ways_count = 0
        if (maze_darray(posi_x+1,posi_y)+4 == 4) ways_count = ways_count + 1
        if (maze_darray(posi_x,posi_y+1)+4 == 4) ways_count = ways_count + 1
        if (maze_darray(posi_x-1,posi_y)+4 == 4) ways_count = ways_count + 1
        if (maze_darray(posi_x,posi_y-1)+4 == 4) ways_count = ways_count + 1

        !!!!!! 如果是的話 我會把x和y 都記錄下來 !!!!
        if (ways_count > 1) then
            intersection_x(intersection_count) = posi_x
            intersection_y(intersection_count) = posi_y
            intersection_count = intersection_count + 1
        endif
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if (maze_darray(posi_x+1,posi_y)+4 == 4) then
            maze_darray(posi_x+1,posi_y) = 4
            posi_x = posi_x + 1
            route_darray(posi_x+1,posi_y) = 1
        else if (maze_darray(posi_x,posi_y+1)+4 == 4) then
            maze_darray(posi_x,posi_y+1) = 4
            posi_y = posi_y + 1
            route_darray(posi_x,posi_y+1) = 1
        else if (maze_darray(posi_x-1,posi_y)+4 == 4) then
            maze_darray(posi_x-1,posi_y) = 4
            posi_x = posi_x - 1
            route_darray(posi_x,posi_y+1) = 1
        else if (maze_darray(posi_x,posi_y-1)+4 == 4) then
            maze_darray(posi_x,posi_y-1) = 4
            posi_y = posi_y - 1
            route_darray(posi_x,posi_y-1) = 1

        !!!!!!!!!!!!!!!! else 這裡 就是回到上一個交叉路口 !!!!!!!!!!!!!!!
        else
            posi_x = intersection_x(intersection_count-1)
            posi_y = intersection_y(intersection_count-1)
            intersection_count = intersection_count - 1
        endif
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        route_count = route_count + 1
        !print*,posi_x,posi_y,intersection_count,route_count
        if ((posi_x == end_x) .and. (posi_y == end_y)) then
            print*,'hell yeah'
            exit
        endif
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!  印出最終的迷宮  !!!!!!!!!!!!!!!!!!!!!

    do i = 1,y_boundary
        do j = 1,x_boundary
            write(*,'(i1)',advance='no') maze_darray(j,i)
        enddo
        print*,
    enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!

        
end