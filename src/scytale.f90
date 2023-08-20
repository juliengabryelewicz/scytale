module scytale
  implicit none
  public :: scytale_crypt, scytale_translate
contains

  function get_index(index) result(new_index)
    integer, optional :: index
    integer :: new_index

    if (present(index)) then
      if (index<=20 .and. index>0) then
        new_index = index 
      else
        new_index = 3
      end if
    else
      new_index = 3
    end if    

  end function get_index

  function scytale_crypt(sentence,index) result(translated)
    character(:), allocatable :: sentence, translated
    integer, optional :: index
    integer :: index2, diameter
    integer :: i,j
    sentence = trim(sentence)
    translated = ""

    index2 = get_index(index)

    do while (mod(len(sentence),index2) /= 0)
      sentence = sentence // " "
    end do

    diameter = len(sentence)/index2

    do i=1,diameter
      do j=0,len(sentence),diameter
        translated = translated // sentence(i+j:i+j)
      end do
    end do

  end function scytale_crypt

  function scytale_translate(sentence,index) result(translated)
    character(:), allocatable :: sentence, translated
    integer, optional :: index
    integer :: index2
    integer :: i,j
    sentence = trim(sentence)
    translated = ""

    index2 = get_index(index)

    do i=1,index2
      do j=0,len(sentence),index2
        translated = translated // sentence(i+j:i+j)
      end do
    end do

  end function scytale_translate


end module scytale
