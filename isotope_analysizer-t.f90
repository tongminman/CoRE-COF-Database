program isotope_analysizer
    
    use dflib
    use FilesSrch_Module
    implicit none
    
    logical(4)::result
    character(1024)::cmd
    integer(4)::FilePort1,Fileport2,FilePort3,FilePort4,FilePort5,FilePort6,FilePort7
    integer(4)::ios1,ios2,ios3,ios4,ios5,ios6,ios7
    character(1024)::FilePath1,FilePath2,FilePath3,FilePath4,FilePath5,FilePath6,FilePath7
    character(1024)::line1,line2,line3,line4,line5,line6,line7
    character(1024)::keyword1,keyword2,keyword3,keyword4,keyword5,keyword6,keyword7,keyword8
    character(1024)::keyword9,keyword10,keyword11,keyword12
    character(1024)::getword1,getword2
    integer(4)::StartPoint,EndPoint,loop_var
    character(1024)::temp_dir,res_dir
    character(100)::FrameworkID,FrameworkName,FrameworkDir,PoreInfo,Dlis,Dlfs,DLisafsp
	character(100)::Mass,density,volume,cpu_time
    character(100)::D2_MPUC,H2_MPUC,SELECT,D2_NONQMFH,H2_NONQMFH,D2_QMFH,H2_QMFH
    character(100)::title,colon
    integer(4)::find_key,total_num,valid_num,invalid_num
    logical(4)::isPore,isTime
    character(20)::str1,str2,str3,str4,str5,str6,str7,str8,str9,str10,str11,str12
    character(20)::tot_time,day,hr,min,sec
    integer(4)::nx,ny,nz
    
    isPore=.false.
    
    keyword1="Framework"
    keyword2="FrameworkDirectoryFileName"
    keyword3="Averaged"
    keyword4="Average_Absolute_Loading_[Molecules_UC]"
    keyword5="NonQMFH_Average_<Qst>"
    keyword6="QMFH_Average_<Qst>"
    keyword7="pore_information:"
    keyword8="Framework_Mass"
    keyword9="Framework_Density"
    keyword10="Total_Volume"
    keyword11="# This Simulaton Job Takes Total Cpu Time :"
    keyword12="Number_Of_UnitCells"

	
    FilePort1=100
    FilePort2=200
    FilePort3=300
    FilePort4=400
    FilePort5=500
    FilePort6=600
    FilePort7=700
    
    FilePath1="../FileControl/folder_range.txt"
    open(unit=FilePort1,file=FilePath1)
    do while(.true.)
        read(FilePort1,"(a1024)",iostat=ios1) line1
        !write(*,*)trim(line1)
        if(ios1/=0) exit
        if(len(trim(line1))/=0) then
            read(line1,*)StartPoint,EndPoint
            !write(*,*)StartPoint,EndPoint
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            temp_dir="../../Files_"//trim(int2str(StartPoint))//"_"//trim(int2str(EndPoint))//"/"
            res_dir="../analysize_result/results_"//trim(int2str(StartPoint))//"_"//trim(int2str(EndPoint))//"/"
            cmd="mkdir ..\analysize_result\results_"//trim(int2str(StartPoint))//"_"//trim(int2str(EndPoint))
            result=systemqq(adjustl(trim(cmd)))
            FilePath4=trim(res_dir)//"invalid_result.txt"
            open(unit=FilePort4,file=FilePath4)
            FilePath5=trim(res_dir)//"results_summation.txt"
            open(unit=FilePort5,file=FilePath5)
            FilePath7=trim(res_dir)//"results_conclusion.txt"
            open(unit=FilePort7,file=FilePath7)
            !write(*,*)trim(temp_dir)
            total_num=0
            valid_num=0
            invalid_num=0
            do loop_var=StartPoint,EndPoint,1
                FilePath2=trim(temp_dir)//trim(int2str(loop_var))//"/"//"StructureName.input"
                open(unit=FilePort2,file=FilePath2)
                do while(.true.)
                    read(FilePort2,"(a1024)",iostat=ios2) line2
                    if(ios2/=0) exit
                    if(len(trim(line2))/=0) then
                        read(line2,*)getword1
                    if(trim(getword1)==trim(keyword1)) then 
                        read(line2,*) getword1,getword2 
                        FrameworkName=trim(getword2)
                        write(*,*)trim(FrameworkName)
                    end if
                    if(trim(getword1)==trim(keyword2)) then
                        read(line2,*) getword1,getword2
                        FrameworkDir=trim(getword2)
                        !write(*,*)trim(FrameworkDir)
                    end if
                    if(isPore==.true. .and. trim(getword1)==trim(keyword7)) then
                        read(line2,*)PoreInfo,Dlis,Dlfs,DLisafsp
                    end if
                    
                    
                    
                    else
                        cycle
                    end if
                end do
                close(FilePort2)
                
                FilePath3=trim(temp_dir)//trim(int2str(loop_var))//"/"//"output/SimuResults.data"
                find_key=0
                total_num=total_num+1
                open(unit=FilePort3,file=FilePath3)
                do while(.true.)
                    read(FilePort3,"(a1024)",iostat=ios3)line3                    
                    if(ios3/=0) exit
                    if(len(trim(line3))/=0) then
                    read(line3,*)getword1
					
                    if(trim(getword1)==trim(keyword12)) then
                        read(Line3,*)title,colon,nx,ny,nz
                    end if
                    
                    if(trim(getword1)==trim(keyword8)) then
                        read(Line3,*)title,colon,Mass
                    end if
                    if(trim(getword1)==trim(keyword9)) then
                        read(Line3,*)title,colon,density
                    end if
                    if(trim(getword1)==trim(keyword10)) then
                        read(Line3,*)title,colon,volume
                    end if					
					
                    if(trim(getword1)==trim(keyword3)) then 
                        find_key=1
                        valid_num=valid_num+1
                        write(FilePort5,"(a)")
                        write(FilePort5,"(a10,1x,a20,1x,a80)")trim(int2str(loop_var)),FrameworkDir,FrameworkName
		                write(FilePort5,*)nx,"  ",ny,"  ",nz
                        write(FilePort5,"(3(a10,1x))")trim(Mass),trim(density),trim(volume)				
                        if(isPore==.true.) write(FilePort5,"(a20,1x,a10,1x,a10,1x,a10)")trim(PoreInfo),trim(Dlis),trim(Dlfs),trim(DLisafsp)
                        end if
                    if(find_key==1) then
                    if(trim(getword1)==trim(keyword4).or.trim(getword1)==trim(keyword5).or.trim(getword1)==trim(keyword6)) then
                            write(FilePort5,"(a)")trim(line3)                            
                    end if                    
                    else
                        cycle                        
                    end if                    
                    end if
                end do
                    if(find_key/=1) then                        
                        invalid_num=invalid_num+1
                        write(FilePort4,"(a10,1x,a20,1x,a80)")trim(int2str(loop_var)),FrameworkDir,FrameworkName
                    end if
                close(FilePort3)   
           end do
            close(FilePort4)
            close(FilePort5)
            write(FilePort7,"(a)")"the total number of frameworks is    : "//trim(int2str(total_num))
            write(FilePort7,"(a)")"the valid number of frameworks is    : "//trim(int2str(valid_num))
            write(FilePort7,"(a)")"the invalid number of frameworks is  : "//trim(int2str(invalid_num))
            if(total_num==valid_num+invalid_num) write(FilePort7,"(a)")"the analysize result is reliable"
            if(total_num/=valid_num+invalid_num) write(FilePort7,"(a)")"the analysize result is unreliable"
            close(FilePort7)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            FilePath6=trim(res_dir)//"valid_results.txt"
            open(unit=FilePort6,file=FilePath6)
            if(not(isPore)) then
            write(FilePort6,"(a10,1x,a20,1x,a80,8(1x,a10),1x,a10)")"FrameworkID","FrameworkDIR","FrameworkName","D2_MPUC","H2_MPUC","SELECTIVITY",&
            "D2_NONQMFH","H2_NONQMFH","M[g/mol]","den[g/cm^3]","vol[A^3]","time[hour]"
            else
            write(FilePort6,"(a10,1x,a20,1x,a80,11(1x,a10),1x,a10)")"FrameworkID","FrameworkDIR","FrameworkName","D2_MPUC","H2_MPUC","SELECTIVITY",&
            "D2_NONQMFH","H2_NONQMFH","Dlis","Dlfs","DLisafsp","M[g/mol]","den[g/cm^3]","vol[A^3]","time[hour]"
            end if
            
            open(unit=FilePort5,file=FilePath5)
            do loop_var=1,valid_num,1
                read(FilePort5,*)
                read(FilePort5,*)FrameworkID,FrameworkDir,FrameworkName
				read(FilePort5,*)nx,ny,nz
                read(FilePort5,*)Mass,density,volume
                if(isPore==.true.) read(FilePort5,*)PoreInfo,Dlis,Dlfs,DLisafsp
                read(FilePort5,*)title,colon,D2_MPUC
                read(FilePort5,*)title,colon,H2_MPUC
                read(FilePort5,*)title,D2_NONQMFH
                read(FilePort5,*)title,H2_NONQMFH
                !read(FilePort5,*)title,D2_QMFH
                !read(FilePort5,*)title,H2_QMFH 
          
                				
                if(not(isPore)) then
            write(FilePort6,"(a10,1x,a20,1x,a80,7(1x,a10),1x,a10)")trim(FrameworkID),trim(FrameworkDir),trim(FrameworkName),trim(D2_MPUC),trim(H2_MPUC),&
            trim(real2str(toreal(D2_MPUC)/toreal(H2_MPUC))),trim(D2_NONQMFH),trim(H2_NONQMFH),&
            trim(Mass),trim(density),trim(real2str(toreal(volume)/(nx*ny*nz+0.0)))
                else
            write(FilePort6,"(a10,1x,a20,1x,a80,11(1x,a10),1x,a10)")trim(FrameworkID),trim(FrameworkDir),trim(FrameworkName),trim(D2_MPUC),trim(H2_MPUC),&
            trim(real2str(toreal(D2_MPUC)/toreal(H2_MPUC))),trim(D2_NONQMFH),trim(H2_NONQMFH),&
            trim(Dlis),trim(Dlfs),trim(DLisafsp),&
            trim(Mass),trim(density),trim(real2str(toreal(volume)/(nx*ny*nz+0.0))),trim(tot_time)
            end if
            end do
            
            close(FilePort5)
            close(FilePort6)  
            
        else
            cycle
        end if
    end do
    close(FilePort1)
        
   write(*,*)"job finished"
   read(*,*)
   end program isotope_analysizer     