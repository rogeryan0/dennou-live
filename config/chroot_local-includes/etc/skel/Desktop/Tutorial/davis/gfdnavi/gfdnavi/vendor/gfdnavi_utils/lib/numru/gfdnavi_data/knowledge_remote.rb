# -*- coding: japanese-cp932 -*-
require "numru/gfdnavi_data/knowledge"

module NumRu::GfdnaviData
  class KnowledgeRemote < NumRu::GfdnaviData::Knowledge

    def to_knlge
      get_object("knlge")
    end

    def initialize(hash={})
      @@figure_paths    = []
      @@figure_captions = []
      super(hash)
    end

    # == KnowledgeRemote�p�̃��\�b�h���`����B
    #    app/models/knowledge.rb �Œ�`���ꂽ���\�b�h���𗘗p����
    meths = ["category","creator","textbody","default_layout",\
             "horizontal_figures","figures_size_height_or_width", \
             "figures_size_units","figures_size_number",\
             "comment_on","comment_number"]
    meths.collect!{|me| /\A(.+)_id\z/ =~ me ? $1 : me}
    meths += ["get_contents", "relational_images", "relational_variables",\
              "figures", "insert_figures", "swap_figures", "delete_figure",\
              "comments", "make_new_comment", "save", "delete"]

    # * ��L�̖��O�ɂ��āA���L�̓��e�Ń��\�b�h���`����
    meths.each do |name|
        eval <<-EOL, binding, __FILE__, __LINE__+1
        def #{name}=(arg)
          get_object("knlge") unless @new_data
          @representation[\"knlge\"] ||= {\"gfdnavi_knowledge\"=>Hash.new}
          @representation[\"knlge\"][\"gfdnavi_knowledge\"][\"#{name}\"] = arg
        end
        EOL

        eval <<-EOL, binding, __FILE__, __LINE__+1
        def #{name}
          unless @representation[\"knlge\"] && @representation[\"knlge\"][\"gfdnavi_knowledge\"]
            return nil
          end
          @representation[\"knlge\"][\"gfdnavi_knowledge\"][\"#{name}\"]
        end
        EOL
    end

    # == �摜�𖖔��ɑ}�����郁�\�b�h
    #    @representation �ɉ摜�� URL �� YAML ���������̂���������
    #    (YAML������͕̂�����̔z����܂Ƃ߂Ĉ�̕�����ɂ��邽��)
    #    �}���͉摜�ꖇ���ƂȂ�B
    def add_figure(image, caption)
      # �������Q�Ƃ��A@@figure_paths �� @@figure_captions �̒��g�ɒǋL���Ă���
      unless caption
        caption = ""
      end

      if String === image
        @@figure_paths    << image
      else
        # @@figure_paths    << image.url
        @@figure_paths    << image.path  # http://�͂��̎��_�ł��Ȃ����Ƃɂ���
        # knowledge��o�^����Ƃ���Ȃ�path�A�����łȂ��Ȃ�url�Ƃ����̂�
        # ���z�̂͂��B
      end
      @@figure_captions << caption

      # Local(�T�[�o)���֕�����𑗂�B
      # �����́ALocal���� figures= ���\�b�h�̈����ƂȂ�(add_figure �ł͂Ȃ��B)
      # Local�̕��ł́A����ꂽ YAML �t�@�C�����Q�Ƃ��A�������̐}����x�ɓ����d�l�ɂȂ��Ă���B
      # Remote �� add_figure ���Ăяo�����x�� @@figure_paths �� @@figure_captions �̒��g��
      # �ǋL����Ă����A���� Hash �̒��g���X�V���Ă����B
      @representation["knlge"] ||= {"gfdnavi_knowledge"=>Hash.new}
      @representation["knlge"]["gfdnavi_knowledge"]["figures"] = String.new
      @representation["knlge"]["gfdnavi_knowledge"]["figures"] = YAML.dump(@@figure_paths + @@figure_captions)
    end

    def update_save_data(hash)
      # * hash ���󂯎�鑤(Local, �T�[�o)�� 
      #   gl.key = value �Ƃ����`�ɂȂ�B
      #   �܂�Akey �Ƃ������\�b�h�ɁA value �Ƃ���������n���B
      #   value �͕�����̂ݑΉ��B
      hash.update(@representation["knlge"]["gfdnavi_knowledge"])
    end
  end
end
